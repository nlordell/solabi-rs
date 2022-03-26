//! Solidity ABI decoding.

use crate::{layout::Layout, types::Word};

/// Represents a decodable type.
pub trait Decode: Layout + Sized {
    /// Writes the type's data to the specified encoder.
    fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError>;
}

/// ABI-decodes a value.
pub fn decode<T>(_bytes: &[u8]) -> Result<T, DecodeError> {
    todo!();
}

/// An ABI decoder
pub struct Decoder<'a> {
    buffer: &'a [u8],
    position: usize,
}

/// An error during decoding.
pub enum DecodeError {
    /// The end of buffer was reached while still decoding.
    EndOfBuffer,
}

impl<'a> Decoder<'a> {
    /// Create a new ABI decoder around the specified buffer.
    fn new(buffer: &'a [u8]) -> Self {
        Self {
            buffer,
            position: 0,
        }
    }

    /// Writes a word to the encoder.
    pub fn read_word(&mut self) -> Word {
        todo!()
    }

    /// Writes a slice of bytes to the encoder.
    pub fn read_bytes(&mut self) -> &'a [u8] {
        todo!()
    }

    /// Slices the buffer at the offset pointed to by the current word.
    ///
    /// This method is used for getting a sub-`Decoder` for reading the contents
    /// of a dynamic type.
    pub fn slice(&mut self) -> Self {
        todo!()
    }

    /// Returns an decoder anchored at the current position.
    pub fn seek(&mut self) -> Self {
        Self::new(&self.buffer[self.position..])
    }

    /// Writes a value to the encoder.
    ///
    /// This method takes care to either encode the value directly for static
    /// types or slice off some section of the "tail" for dynamic types.
    pub fn read<T>(&mut self) -> Result<T, DecodeError>
    where
        T: Decode,
    {
        todo!()
    }
}
