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
    function::Selector,
    log::{Log, Topics},
    primitive::Word,
};
use sha3::{Digest as _, Keccak256};
use std::borrow::Cow;

/// An error indicating that some value data is not of the correct type.
#[derive(Debug)]
pub struct ValueKindError;

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
    pub fn new(descriptor: &EventDescriptor) -> Result<Self, ValueKindError> {
        let selector = descriptor.selector();
        if selector.iter().count() + descriptor.inputs.iter().filter(|i| i.indexed).count()
            > Topics::MAX_LEN
        {
            return Err(ValueKindError);
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

    /// Encodes a Solidity error for the specified data.
    pub fn encode(&self, fields: &[Value]) -> Result<Log<'static>, ValueKindError> {
        of_kind(fields, self.fields.iter().map(|(_, kind)| kind))?;
        let encoding = EncodeLog(&self.fields, fields);
        let data = crate::encode(&encoding);

        let mut topics = Topics::default();
        if let Some(selector) = self.selector {
            topics.push(selector)
        }

        let (head, _) = encoding.size().word_count();
        let mut tail_offset = head;

        for ((indexed, _), value) in self.fields.iter().zip(fields) {
            if !indexed {
                if let Size::Dynamic(head, tail) = value.size() {
                    tail_offset += head + tail;
                }
                continue;
            }

            if let Some(word) = value.to_word() {
                topics.push(word);
            } else {
                let count = value.size().total_word_count();
                let digest = hash_data(&data, tail_offset, count);
                topics.push(digest);
            }
        }

        Ok(Log {
            topics,
            data: Cow::Owned(data),
        })
    }

    /// Decodes a Solidity error from the return bytes call into its data.
    pub fn decode(&self, log: &Log) -> Result<Vec<Value>, DecodeError> {
        let mut topics = log.topics.into_iter();
        if let Some(selector) = self.selector {
            if !matches!(topics.next(), Some(topic) if topic == selector) {
                return Err(DecodeError::InvalidData);
            }
        }

        let mut fields = context::decode::<DecodeLog>(&log.data, &self.fields)?.0;
        for (((_, kind), value), topic) in self
            .fields
            .iter()
            .zip(&mut fields)
            .filter(|((indexed, kind), _)| *indexed && kind.is_primitive())
            .zip(topics)
        {
            *value = Value::from_word(kind, topic).unwrap();
        }

        Ok(fields)
    }
}

/// Internal type for encoding log data, skipping indexed fields.
struct EncodeLog<'a>(&'a [(bool, ValueKind)], &'a [Value]);

impl EncodeLog<'_> {
    fn values(&self) -> impl Iterator<Item = &'_ Value> + '_ {
        self.0
            .iter()
            .zip(self.1)
            .filter(|((indexed, kind), _)| !indexed || !kind.is_primitive())
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
            .any(|(_, kind)| Decodable::is_dynamic_context(kind))
    }

    fn decode_context(decoder: &mut Decoder, context: &Self::Context) -> Result<Self, DecodeError> {
        Ok(Self(
            context
                .iter()
                .map(|(indexed, kind)| {
                    if *indexed && kind.is_primitive() {
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

fn hash_data(data: &[u8], offset: usize, count: usize) -> Word {
    let mut hasher = Keccak256::new();
    hasher.update(&data[offset * 32..(offset + count) * 32]);
    hasher.finalize().into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Uint;
    use ethaddr::address;
    use ethnum::U256;
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
            Value::Uint(Uint::new(256, U256::new(4_200_000_000_000_000_000)).unwrap()),
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
            Value::Uint(Uint::new(256, U256::new(4_200_000_000_000_000_000)).unwrap()),
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
    fn anonymous_event_with_indexed_dynamic_field() {
        let event =
            EventDescriptor::parse_declaration("event Log(uint, string indexed, uint) anonymous")
                .unwrap();
        let encoder = EventEncoder::new(&event).unwrap();

        let fields = [
            Value::Uint(Uint::new(256, U256::new(1)).unwrap()),
            Value::String("hello world".to_owned()),
            Value::Uint(Uint::new(256, U256::new(2)).unwrap()),
        ];

        let log = Log {
            topics: Topics::from([keccak256(&hex!(
                "000000000000000000000000000000000000000000000000000000000000000b
                 68656c6c6f20776f726c64000000000000000000000000000000000000000000"
            ))]),
            data: hex!(
                "0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000060
                 0000000000000000000000000000000000000000000000000000000000000002
                 000000000000000000000000000000000000000000000000000000000000000b
                 68656c6c6f20776f726c64000000000000000000000000000000000000000000"
            )[..]
                .into(),
        };

        assert_eq!(encoder.encode(&fields).unwrap(), log);
        assert_eq!(encoder.decode(&log).unwrap(), fields);

        // Note that we don't actually verify the log topics for dynamic data!
        let log = Log {
            topics: Topics::from([hex!(
                "0000000000000000000000000000000000000000000000000000000000000000"
            )]),
            ..log
        };
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

    fn keccak256(data: &[u8]) -> Word {
        let mut hasher = Keccak256::new();
        hasher.update(data);
        hasher.finalize().into()
    }
}
