//! Module containing various encoder implementations for `Value` Solidity ABI
//! elements.

use super::{Decodable, Encodable, Value, ValueKind};
use crate::{
    abi::{ConstructorDescriptor, ErrorDescriptor, EventDescriptor, FunctionDescriptor},
    decode::{
        context::{self, DecodeContext},
        DecodeError, Decoder,
    },
    encode::{Encode, Encoder, Size},
    event::ParseError,
    function::Selector,
    log::{Log, Topics},
    primitive::Word,
};
use std::{
    borrow::Cow,
    error::Error,
    fmt::{self, Display, Formatter},
};

/// A function encoder.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionEncoder {
    selector: Selector,
    params: Vec<ValueKind>,
    returns: Vec<ValueKind>,
}

impl FunctionEncoder {
    /// Creates a new function encoder for the specified ABI descriptor.
    pub fn new(descriptor: &FunctionDescriptor) -> Self {
        Self {
            selector: descriptor.selector(),
            params: descriptor
                .inputs
                .iter()
                .map(|i| i.field.kind.clone())
                .collect(),
            returns: descriptor
                .outputs
                .iter()
                .map(|i| i.field.kind.clone())
                .collect(),
        }
    }

    /// Encodes a function call for the specified parameters.
    pub fn encode_params(&self, params: &[Value]) -> Result<Vec<u8>, ValueKindError> {
        of_kind(params, &self.params)?;
        Ok(Value::encode_tuple_with_selector(self.selector, params))
    }

    /// Decodes a function call into its parameters.
    pub fn decode_params(&self, data: &[u8]) -> Result<Vec<Value>, DecodeError> {
        Value::decode_tuple_with_selector(&self.params, self.selector, data)
    }

    /// Encodes function return data.
    pub fn encode_returns(&self, returns: &[Value]) -> Result<Vec<u8>, ValueKindError> {
        of_kind(returns, &self.returns)?;
        Ok(Value::encode_tuple(returns))
    }

    /// Decodes function return data.
    pub fn decode_returns(&self, data: &[u8]) -> Result<Vec<Value>, DecodeError> {
        Value::decode_tuple(&self.returns, data)
    }
}

/// A constructor encoder.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ConstructorEncoder {
    code: Vec<u8>,
    params: Vec<ValueKind>,
}

impl ConstructorEncoder {
    /// Creates a new constructor encoder from a selector.
    pub fn new(code: Vec<u8>, descriptor: &ConstructorDescriptor) -> Self {
        Self {
            code,
            params: descriptor
                .inputs
                .iter()
                .map(|i| i.field.kind.clone())
                .collect(),
        }
    }

    /// Encodes a contract deployment for the specified parameters.
    pub fn encode(&self, params: &[Value]) -> Result<Vec<u8>, ValueKindError> {
        of_kind(params, &self.params)?;
        Ok(Value::encode_tuple_with_prefix(&self.code, params))
    }

    /// Encodes a contract deployment parameters without any code.
    pub fn encode_params(&self, params: &[Value]) -> Result<Vec<u8>, ValueKindError> {
        of_kind(params, &self.params)?;
        Ok(Value::encode_tuple(params))
    }

    /// Decodes the contract deployment parameters from the specified calldata.
    pub fn decode(&self, data: &[u8]) -> Result<Vec<Value>, DecodeError> {
        Value::decode_tuple_with_prefix(&self.params, &self.code, data)
    }

    /// Decodes the contract deployment parameters without any code.
    pub fn decode_params(&self, data: &[u8]) -> Result<Vec<Value>, DecodeError> {
        Value::decode_tuple(&self.params, data)
    }
}

/// An event encoder.
pub struct EventEncoder {
    selector: Option<Word>,
    fields: Vec<(bool, ValueKind)>,
}

impl EventEncoder {
    /// Creates a new error encoder from a selector.
    pub fn new(descriptor: &EventDescriptor) -> Result<Self, EventIndexError> {
        let selector = descriptor.selector();
        if selector.iter().count() + descriptor.inputs.iter().filter(|i| i.indexed).count()
            > Topics::MAX_LEN
        {
            return Err(EventIndexError);
        }

        Ok(Self {
            selector,
            fields: descriptor
                .inputs
                .iter()
                .map(|i| (i.indexed, i.field.kind.clone()))
                .collect(),
        })
    }

    /// Encodes a Solidity event for the specified Ethereum log.
    pub fn encode(&self, fields: &[Value]) -> Result<Log<'static>, ValueKindError> {
        of_kind(fields, self.fields.iter().map(|(_, kind)| kind))?;
        let encoding = EncodeLog(&self.fields, fields);
        let data = crate::encode(&encoding);

        let mut topics = Topics::default();
        if let Some(selector) = self.selector {
            topics.push_word(selector)
        }

        for (_, value) in self
            .fields
            .iter()
            .zip(fields)
            .filter(|((indexed, _), _)| *indexed)
        {
            topics.push_word(value.to_topic());
        }

        Ok(Log {
            topics,
            data: Cow::Owned(data),
        })
    }

    /// Decodes a Solidity event from an Ethereum log.
    ///
    /// Note that non-primitive indexed fields will be replaced with a `bytes32`
    /// value equal to the hash of its ABI-encoded value.
    pub fn decode(&self, log: &Log) -> Result<Vec<Value>, ParseError> {
        if log.topics.len() != self.topic_count() {
            return Err(ParseError::Index);
        }

        let mut topics = log.topics.into_iter();
        if let Some(selector) = self.selector {
            let topic0 = topics.next().expect("unexpected missing topic");
            if topic0 != selector {
                return Err(ParseError::SelectorMismatch(topic0));
            }
        }

        let mut fields = context::decode::<DecodeLog>(&log.data, &self.fields)?.0;
        for (((_, kind), value), topic) in self
            .fields
            .iter()
            .zip(&mut fields)
            .filter(|((indexed, _), _)| *indexed)
            .zip(topics)
        {
            *value = if kind.is_primitive() {
                Value::from_word(kind, topic).unwrap()
            } else {
                Value::FixedBytes(topic.into())
            };
        }

        Ok(fields)
    }

    /// Decodes a Solidity event from an Ethereum log, replacing non-primitive
    /// indexed fields with their default values (e.g. empty string for a
    /// `string` field).
    pub fn decode_lossy(&self, log: &Log) -> Result<Vec<Value>, ParseError> {
        let mut fields = self.decode(log)?;
        for ((_, kind), value) in self
            .fields
            .iter()
            .zip(&mut fields)
            .filter(|((indexed, kind), _)| *indexed && !kind.is_primitive())
        {
            *value = Value::default(kind);
        }
        Ok(fields)
    }

    /// Returns the number of topics of an Ethereum log that encodes this event.
    fn topic_count(&self) -> usize {
        (self.selector.is_some() as usize)
            + self.fields.iter().filter(|(indexed, _)| *indexed).count()
    }
}

/// Internal type for encoding log data, skipping indexed fields.
struct EncodeLog<'a>(&'a [(bool, ValueKind)], &'a [Value]);

impl EncodeLog<'_> {
    fn values(&self) -> impl Iterator<Item = &'_ Value> + '_ {
        self.0
            .iter()
            .zip(self.1)
            .filter(|((indexed, _), _)| !indexed)
            .map(|(_, value)| value)
    }
}

impl Encode for EncodeLog<'_> {
    fn size(&self) -> Size {
        Size::tuple(self.values().map(|item| Encodable(item).size()))
    }

    fn encode(&self, encoder: &mut Encoder) {
        for value in self.values() {
            encoder.write(&Encodable(value))
        }
    }
}

/// Internal type for decoding log data, skipping indexed fields.
struct DecodeLog(Vec<Value>);

impl DecodeContext for DecodeLog {
    type Context = [(bool, ValueKind)];

    fn is_dynamic_context(context: &Self::Context) -> bool {
        context
            .iter()
            .filter(|(indexed, _)| !indexed)
            .any(|(_, kind)| Decodable::is_dynamic_context(kind))
    }

    fn decode_context(decoder: &mut Decoder, context: &Self::Context) -> Result<Self, DecodeError> {
        Ok(Self(
            context
                .iter()
                .map(|(indexed, kind)| {
                    if *indexed {
                        Ok(Value::default(kind))
                    } else {
                        Ok(decoder.read_context::<Decodable>(kind)?.0)
                    }
                })
                .collect::<Result<_, _>>()?,
        ))
    }
}

/// An error encoder.
pub struct ErrorEncoder {
    selector: Selector,
    fields: Vec<ValueKind>,
}

impl ErrorEncoder {
    /// Creates a new error encoder from a selector.
    pub fn new(descriptor: &ErrorDescriptor) -> Self {
        Self {
            selector: descriptor.selector(),
            fields: descriptor.inputs.iter().map(|i| i.kind.clone()).collect(),
        }
    }

    /// Encodes a Solidity error for the specified data.
    pub fn encode(&self, fields: &[Value]) -> Result<Vec<u8>, ValueKindError> {
        of_kind(fields, &self.fields)?;
        Ok(Value::encode_tuple_with_selector(self.selector, fields))
    }

    /// Decodes a Solidity error from the return bytes call into its data.
    pub fn decode(&self, data: &[u8]) -> Result<Vec<Value>, DecodeError> {
        Value::decode_tuple_with_selector(&self.fields, self.selector, data)
    }
}

fn of_kind<'a, I>(values: &'a [Value], kinds: I) -> Result<(), ValueKindError>
where
    I: IntoIterator<Item = &'a ValueKind>,
    I::IntoIter: ExactSizeIterator,
{
    let kinds = kinds.into_iter();
    if values.len() != kinds.len()
        || values
            .iter()
            .zip(kinds)
            .any(|(value, kind)| !value.is_kind(kind))
    {
        return Err(ValueKindError);
    }
    Ok(())
}

/// An error indicating that some value data is not of the correct type.
#[derive(Debug)]
pub struct ValueKindError;

impl Display for ValueKindError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("value does not match the expected kind")
    }
}

impl Error for ValueKindError {}

/// An error indicating that an event descriptor contains too many indexed
/// fields.
#[derive(Debug)]
pub struct EventIndexError;

impl Display for EventIndexError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("event has too many indexed fields")
    }
}

impl Error for EventIndexError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::{Array, Uint};
    use ethprim::{address, uint, U256};
    use hex_literal::hex;

    #[test]
    fn transfer_function_encoding() {
        let function = FunctionDescriptor::parse_declaration(
            "function transfer(address to, uint value) returns (bool)",
        )
        .unwrap();
        let encoder = FunctionEncoder::new(&function);

        let params = [
            Value::Address(address!("0x0101010101010101010101010101010101010101")),
            Value::Uint(Uint::new(256, uint!("4_200_000_000_000_000_000")).unwrap()),
        ];

        let call = hex!(
            "a9059cbb
             0000000000000000000000000101010101010101010101010101010101010101
             0000000000000000000000000000000000000000000000003a4965bf58a40000"
        );

        assert_eq!(encoder.encode_params(&params).unwrap(), call);
        assert_eq!(encoder.decode_params(&call).unwrap(), params);

        let returns = [Value::Bool(true)];

        let ret = hex!("0000000000000000000000000000000000000000000000000000000000000001");

        assert_eq!(encoder.encode_returns(&returns).unwrap(), ret);
        assert_eq!(encoder.decode_returns(&ret).unwrap(), returns);
    }

    #[test]
    fn proxy_constructor() {
        let constructor =
            ConstructorDescriptor::parse_declaration("constructor(address implementation)")
                .unwrap();
        let code = hex!(
            "60a060405234801561001057600080fd5b506040516101083803806101088339
             8101604081905261002f91610040565b6001600160a01b031660805261007056
             5b60006020828403121561005257600080fd5b81516001600160a01b03811681
             1461006957600080fd5b9392505050565b608051608061008860003960006006
             015260806000f3fe60806040527f000000000000000000000000000000000000
             00000000000000000000000000003660008037600080366000845af43d600080
             3e80600181146045573d6000fd5b3d6000f3fea264697066735822122007589b
             6aeb4b41bc48a82fc5939d02ccb42a23fd27c8bf5430706f182fd9a47164736f
             6c63430008100033"
        );
        let encoder = ConstructorEncoder::new(code.to_vec(), &constructor);

        let params = [Value::Address(address!(
            "0x0101010101010101010101010101010101010101"
        ))];

        let data = hex!("0000000000000000000000000101010101010101010101010101010101010101");
        let call = [&code[..], &data[..]].concat();

        assert_eq!(encoder.encode(&params).unwrap(), call);
        assert_eq!(encoder.encode_params(&params).unwrap(), data);
        assert_eq!(encoder.decode(&call).unwrap(), params);
        assert_eq!(encoder.decode_params(&data).unwrap(), params);
    }

    #[test]
    fn transfer_event() {
        let event = EventDescriptor::parse_declaration(
            "event Transfer(address indexed to, address indexed from, uint256 value)",
        )
        .unwrap();
        let encoder = EventEncoder::new(&event).unwrap();

        let fields = [
            Value::Address(address!("0x0101010101010101010101010101010101010101")),
            Value::Address(address!("0x0202020202020202020202020202020202020202")),
            Value::Uint(Uint::new(256, uint!("4_200_000_000_000_000_000")).unwrap()),
        ];

        let log = Log {
            topics: Topics::from([
                event.selector().unwrap(),
                hex!("0000000000000000000000000101010101010101010101010101010101010101"),
                hex!("0000000000000000000000000202020202020202020202020202020202020202"),
            ]),
            data: hex!("0000000000000000000000000000000000000000000000003a4965bf58a40000")[..]
                .into(),
        };

        assert_eq!(encoder.encode(&fields).unwrap(), log);
        assert_eq!(encoder.decode(&log).unwrap(), fields);
    }

    #[test]
    fn fails_to_decode_event_with_different_indices() {
        let decl = |s: &str| {
            let event = EventDescriptor::parse_declaration(s).unwrap();
            let encoder = EventEncoder::new(&event).unwrap();
            (event, encoder)
        };

        let (event, encoder) =
            decl("event Transfer(address indexed to, address indexed from, uint256 value)");

        let selector = event.selector().unwrap();
        let fields = [
            Value::Address(address!("0x0101010101010101010101010101010101010101")),
            Value::Address(address!("0x0202020202020202020202020202020202020202")),
            Value::Uint(Uint::new(256, uint!("4_200_000_000_000_000_000")).unwrap()),
        ];
        let log = encoder.encode(&fields).unwrap();

        // Now try and parse the log to an event with the similar signature, but
        // with a different set of indexed fields.
        for signature in [
            "event Transfer(address indexed to, address from, uint256 value)",
            "event Transfer(address indexed to, address indexed from, uint256 indexed value)",
        ] {
            let (event, encoder) = decl(signature);
            assert_eq!(selector, event.selector().unwrap());
            assert!(matches!(encoder.decode(&log), Err(ParseError::Index)));
        }
    }

    #[test]
    fn fails_to_decode_event_with_missing_topic0() {
        let event = EventDescriptor::parse_declaration("event Foo()").unwrap();
        let encoder = EventEncoder::new(&event).unwrap();

        assert!(matches!(
            encoder.decode(&Log::default()),
            Err(ParseError::Index),
        ));
    }

    #[test]
    fn anonymous_event_with_indexed_dynamic_field() {
        let event = EventDescriptor::parse_declaration(
            r#"
            event Log(
                uint,
                string indexed,
                bool indexed,
                (uint, (bool, bytes))[] indexed,
                uint
            ) anonymous
            "#,
        )
        .unwrap();
        let encoder = EventEncoder::new(&event).unwrap();

        let mut fields = [
            Value::Uint(Uint::new(256, uint!("1")).unwrap()),
            Value::String("hello world".to_owned()),
            Value::Bool(true),
            Value::Array(
                Array::from_values(vec![
                    Value::Tuple(vec![
                        Value::Uint(Uint::new(256, U256::MAX - 1).unwrap()),
                        Value::Tuple(vec![
                            Value::Bool(true),
                            Value::Bytes(hex!("010203").to_vec()),
                        ]),
                    ]),
                    Value::Tuple(vec![
                        Value::Uint(Uint::new(256, U256::MAX - 2).unwrap()),
                        Value::Tuple(vec![
                            Value::Bool(true),
                            Value::Bytes(hex!("040506").to_vec()),
                        ]),
                    ]),
                ])
                .unwrap(),
            ),
            Value::Uint(Uint::new(256, uint!("2")).unwrap()),
        ];

        let log = Log {
            topics: Topics::from([
                hex!("47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"),
                hex!("0000000000000000000000000000000000000000000000000000000000000001"),
                hex!("6b8a0e75eceddd0e7d4d0413a720bce2cb899061e362357db170c49c5563672f"),
            ]),
            data: hex!(
                "0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000002"
            )[..]
                .into(),
        };

        assert_eq!(encoder.encode(&fields).unwrap(), log);

        // Note that indexed dynamic fields are **not** actually recoverable.

        fields[1] = Value::default(&fields[1].kind());
        fields[3] = Value::default(&fields[3].kind());
        assert_eq!(encoder.decode_lossy(&log).unwrap(), fields);

        fields[1] = Value::FixedBytes(
            hex!("47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad").into(),
        );
        fields[3] = Value::FixedBytes(
            hex!("6b8a0e75eceddd0e7d4d0413a720bce2cb899061e362357db170c49c5563672f").into(),
        );
        assert_eq!(encoder.decode(&log).unwrap(), fields);
    }

    #[test]
    fn event_with_bytes() {
        let event = EventDescriptor::parse_declaration(
            "event OrderRefund(bytes orderUid, address indexed refunder)",
        )
        .unwrap();
        let encoder = EventEncoder::new(&event).unwrap();

        let fields = [
            Value::Bytes(
                hex!(
                    "6ec84b0a4a85f4d619359c2c89e14ea1184e4a865a029b7d8f4487cab33354d8
                     40a50cf069e992aa4536211b23f286ef88752187ffffffff"
                )
                .to_vec(),
            ),
            Value::Address(address!(
                ~"0x0214ae5fd178986fa18ff792e0b995dc6a78cd56"
            )),
        ];

        let log = Log {
            topics: Topics::from([
                hex!("195271068a288191e4b265c641a56b9832919f69e9e7d6c2f31ba40278aeb85a"),
                hex!("0000000000000000000000000214ae5fd178986fa18ff792e0b995dc6a78cd56"),
            ]),
            data: hex!(
                "0000000000000000000000000000000000000000000000000000000000000020
                 0000000000000000000000000000000000000000000000000000000000000038
                 6ec84b0a4a85f4d619359c2c89e14ea1184e4a865a029b7d8f4487cab33354d8
                 40a50cf069e992aa4536211b23f286ef88752187ffffffff0000000000000000"
            )[..]
                .into(),
        };

        assert_eq!(encoder.encode(&fields).unwrap(), log);
        assert_eq!(encoder.decode(&log).unwrap(), fields);
    }

    #[test]
    fn revert_error() {
        let error = ErrorDescriptor::parse_declaration("error Error(string message)").unwrap();
        let encoder = ErrorEncoder::new(&error);

        let fields = [Value::String("revert".to_owned())];

        let data = hex!(
            "08c379a0
             0000000000000000000000000000000000000000000000000000000000000020
             0000000000000000000000000000000000000000000000000000000000000006
             7265766572740000000000000000000000000000000000000000000000000000"
        );

        assert_eq!(encoder.encode(&fields).unwrap(), data);
        assert_eq!(encoder.decode(&data).unwrap(), fields);
    }
}
