//! Module containing various encoder implementations for `Value` Solidity ABI
//! elements.

use super::{Decodable, Encodable, Value, ValueKind};
use crate::{
    abi::{ConstructorDescriptor, ErrorDescriptor, FunctionDescriptor},
    decode::{
        context::{self, DecodeContext},
        DecodeError, Decoder,
    },
    encode::{Encode, Encoder, Size},
    function::Selector,
};

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
        Ok(crate::encode_with_selector(
            self.selector,
            &TupleRef(params),
        ))
    }

    /// Decodes a function call into its parameters.
    pub fn decode_params(&self, data: &[u8]) -> Result<Vec<Value>, DecodeError> {
        Ok(context::decode_with_selector::<Tuple>(self.selector, data, &self.params)?.0)
    }

    /// Encodes function return data.
    pub fn encode_returns(&self, returns: &[Value]) -> Result<Vec<u8>, ValueKindError> {
        of_kind(returns, &self.returns)?;
        Ok(crate::encode(&TupleRef(returns)))
    }

    /// Decodes function return data.
    pub fn decode_returns(&self, data: &[u8]) -> Result<Vec<Value>, DecodeError> {
        Ok(context::decode::<Tuple>(data, &self.returns)?.0)
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
        Ok(crate::encode_with_prefix(&self.code, &TupleRef(params)))
    }

    /// Encodes a contract deployment parameters without any code.
    pub fn encode_params(&self, params: &[Value]) -> Result<Vec<u8>, ValueKindError> {
        of_kind(params, &self.params)?;
        Ok(crate::encode(&TupleRef(params)))
    }

    /// Decodes the contract deployment parameters from the specified calldata.
    pub fn decode(&self, data: &[u8]) -> Result<Vec<Value>, DecodeError> {
        Ok(context::decode_with_prefix::<Tuple>(&self.code, data, &self.params)?.0)
    }

    /// Decodes the contract deployment parameters without any code.
    pub fn decode_params(&self, data: &[u8]) -> Result<Vec<Value>, DecodeError> {
        Ok(context::decode::<Tuple>(data, &self.params)?.0)
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
        Ok(crate::encode_with_selector(
            self.selector,
            &TupleRef(fields),
        ))
    }

    /// Decodes a Solidity error from the return bytes call into its data.
    pub fn decode(&self, data: &[u8]) -> Result<Vec<Value>, DecodeError> {
        Ok(context::decode_with_selector::<Tuple>(self.selector, data, &self.fields)?.0)
    }
}

/// An internal type to facilitate encoding tuples by slice of their fields.
struct TupleRef<'a>(&'a [Value]);

impl Encode for TupleRef<'_> {
    fn size(&self) -> Size {
        Size::tuple(self.0.iter().map(|item| Encodable(item).size()))
    }

    fn encode(&self, encoder: &mut Encoder) {
        for value in self.0 {
            encoder.write(&Encodable(value))
        }
    }
}

/// An internal type used to facilisate decoding tuples without having to unwrap
/// the parent `Value`.
struct Tuple(Vec<Value>);

impl DecodeContext for Tuple {
    type Context = Vec<ValueKind>;

    fn is_dynamic_context(context: &Self::Context) -> bool {
        context.iter().any(Decodable::is_dynamic_context)
    }

    fn decode_context(decoder: &mut Decoder, context: &Self::Context) -> Result<Self, DecodeError> {
        Ok(Self(
            context
                .iter()
                .map(|kind| Ok(decoder.read_context::<Decodable>(kind)?.0))
                .collect::<Result<_, _>>()?,
        ))
    }
}

fn of_kind(values: &[Value], kinds: &[ValueKind]) -> Result<(), ValueKindError> {
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

    /*
    #[test]
    fn event_selector() {
        let event = EventDescriptor::parse_declaration(
            "event Transfer(address indexed to, address indexed from, uint256 value)",
        )
        .unwrap();
        assert_eq!(
            event.selector().unwrap(),
            hex!("ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"),
        );
    }
    */

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
