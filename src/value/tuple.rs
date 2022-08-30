//! Module implementing additiona tuple encoding associated functions.
//!
//! This allows tuples to be encoded by references to their fields instead of
//! requiring an owned `Value::Tuple` value.

use super::{Decodable, Encodable, Value, ValueKind};
use crate::{
    decode::{
        context::{self, DecodeContext},
        DecodeError, Decoder,
    },
    encode::{BufferSizeError, Encode, Encoder, Size},
    function::Selector,
};

impl Value {
    /// Encodes a tuple by references to its fields.
    pub fn encode_tuple(fields: &[Self]) -> Vec<u8> {
        crate::encode(&TupleRef(fields))
    }

    /// Encodes a `Value` with a function selector.
    ///
    /// See [`Value::encode`] for more details.
    pub fn encode_tuple_with_selector(selector: Selector, fields: &[Self]) -> Vec<u8> {
        crate::encode_with_selector(selector, &TupleRef(fields))
    }

    /// Encodes a `Value` with a prefix.
    ///
    /// See [`Value::encode`] for more details.
    pub fn encode_tuple_with_prefix(prefix: &[u8], fields: &[Self]) -> Vec<u8> {
        crate::encode_with_prefix(prefix, &TupleRef(fields))
    }

    /// Encodes a `Value` to the specified buffer.
    ///
    /// See [`Value::encode`] for more details.
    pub fn encode_tuple_to(fields: &[Self], buffer: &mut [u8]) -> Result<(), BufferSizeError> {
        crate::encode_to(buffer, &TupleRef(fields))
    }

    /// Decodes a `Value`.
    ///
    /// Note that `Value`s can't use the [`crate::decode`] method directly as
    /// it requires runtime type information for proper decoding.
    pub fn decode_tuple(kinds: &[ValueKind], bytes: &[u8]) -> Result<Vec<Self>, DecodeError> {
        dbg!(kinds, bytes);
        Ok(context::decode::<Tuple>(bytes, kinds)?.0)
    }

    /// Decodes a `Value` from data prefixed with a selector.
    ///
    /// See [`Value::decode`] for more details.
    pub fn decode_tuple_with_selector(
        kinds: &[ValueKind],
        selector: Selector,
        bytes: &[u8],
    ) -> Result<Vec<Self>, DecodeError> {
        Ok(context::decode_with_selector::<Tuple>(selector, bytes, kinds)?.0)
    }

    /// Decodes a `Value` from data with an expected byte prefix.
    ///
    /// See [`Value::decode`] for more details.
    pub fn decode_tuple_with_prefix(
        kinds: &[ValueKind],
        prefix: &[u8],
        bytes: &[u8],
    ) -> Result<Vec<Self>, DecodeError> {
        Ok(context::decode_with_prefix::<Tuple>(prefix, bytes, kinds)?.0)
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
    type Context = [ValueKind];

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
