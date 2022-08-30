//! Module implementing contextual decoding.
//!
//! This allows values to be decoded that rely on runtime type information.

use super::{DecodeError, Decoder};
use crate::function::Selector;

/// Represents a decodable type requing runtime context for decoding.
pub trait DecodeContext: Sized {
    /// Context for decoding.
    type Context: ?Sized;

    /// Returns `true` if the type is dynamic.
    fn is_dynamic_context(context: &Self::Context) -> bool;

    /// Writes the type's data to the specified encoder.
    fn decode_context(decoder: &mut Decoder, context: &Self::Context) -> Result<Self, DecodeError>;
}

/// ABI-decodes a value.
pub fn decode<T>(bytes: &[u8], context: &T::Context) -> Result<T, DecodeError>
where
    T: DecodeContext,
{
    let mut decoder = Decoder::new(bytes);

    // Make sure to call `decode` on the type instead of `read`. This is
    // because the top level tuple that gets encoded never gets redirected
    // even if it is a dynamic type.
    T::decode_context(&mut decoder, context)
}

/// ABI-decodes a value prefixed with a selector.
pub fn decode_with_selector<T>(
    selector: Selector,
    bytes: &[u8],
    context: &T::Context,
) -> Result<T, DecodeError>
where
    T: DecodeContext,
{
    decode_with_prefix(&*selector, bytes, context)
}

/// ABI-decodes a value prefixed with a prefix.
pub fn decode_with_prefix<T>(
    prefix: &[u8],
    bytes: &[u8],
    context: &T::Context,
) -> Result<T, DecodeError>
where
    T: DecodeContext,
{
    let data = bytes.strip_prefix(prefix).ok_or(DecodeError::InvalidData)?;
    decode(data, context)
}

impl<'a> Decoder<'a> {
    /// Reads a value to the decoder for the specified context.
    pub fn read_context<T>(&mut self, context: &T::Context) -> Result<T, DecodeError>
    where
        T: DecodeContext,
    {
        if T::is_dynamic_context(context) {
            T::decode_context(&mut self.slice()?, context)
        } else {
            T::decode_context(self, context)
        }
    }
}
