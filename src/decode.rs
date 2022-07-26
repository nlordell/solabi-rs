//! Solidity ABI decoding.

use crate::primitive::{Primitive, Word};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    mem::MaybeUninit,
};

/// Represents a decodable type.
pub trait Decode: Sized {
    /// Writes the type's data to the specified encoder.
    fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError>;
}

/// ABI-decodes a value.
pub fn decode<T>(bytes: &[u8]) -> Result<T, DecodeError>
where
    T: Decode,
{
    let mut decoder = Decoder::new(bytes);
    decoder.read()
}

/// An ABI decoder
pub struct Decoder<'a> {
    buffer: &'a [u8],
    position: usize,
}

impl<'a> Decoder<'a> {
    /// Create a new ABI decoder around the specified buffer.
    fn new(buffer: &'a [u8]) -> Self {
        Self {
            buffer,
            position: 0,
        }
    }

    /// Reads a word from the decoder.
    pub fn read_word(&mut self) -> Result<Word, DecodeError> {
        Ok(self.read_bytes(32)?.try_into().unwrap())
    }

    /// Reads a slice of bytes to the decoder.
    pub fn read_bytes(&mut self, len: usize) -> Result<&'a [u8], DecodeError> {
        let bytes = &self.buffer[self.position..]
            .get(..len)
            .ok_or(DecodeError::EndOfBuffer)?;
        self.position += len;
        Ok(bytes)
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

    /// Reads a value to the decoder.
    ///
    /// This method takes care to either decode the value directly for static
    /// types or slice off some section of the "tail" for dynamic types.
    pub fn read<T>(&mut self) -> Result<T, DecodeError>
    where
        T: Decode,
    {
        T::decode(self)
    }
}

/// An error during decoding.
#[derive(Debug)]
pub enum DecodeError {
    /// The end of buffer was reached while still decoding.
    EndOfBuffer,
}

impl Display for DecodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DecodeError::EndOfBuffer => f.write_str("unexpected end of buffer while decoding"),
        }
    }
}

impl Error for DecodeError {}

impl<T> Decode for T
where
    T: Primitive,
{
    fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError> {
        Ok(T::cast(decoder.read_word()?))
    }
}

impl<T, const N: usize> Decode for [T; N]
where
    T: Decode,
{
    fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError> {
        // SAFETY: Copy of the unstable standard library implementation of
        // `MaybeUninit::uninit_array()`.
        let mut result: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };

        for element in result.iter_mut() {
            element.write(decoder.read()?);
        }

        // SAFETY: Copy of the unstable standard library implementation of
        // `MaybeUninit::array_assume_init()` noting that `Decode` is not
        // implemented for any uninhabited types (like `!`).
        Ok(unsafe { (&result as *const _ as *const [T; N]).read() })
    }
}

impl<T> Decode for Vec<T>
where
    T: Decode,
{
    fn decode(_decoder: &mut Decoder) -> Result<Self, DecodeError> {
        todo!()
    }
}
