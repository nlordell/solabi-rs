//! Solidity ABI decoding.

use crate::primitive::{Primitive, Word};
use ethnum::U256;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    mem::MaybeUninit,
    str,
};

/// Represents a decodable type.
pub trait Decode: Sized {
    /// Returns `true` if the type is dynamic.
    fn is_dynamic() -> bool;

    /// Writes the type's data to the specified encoder.
    fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError>;
}

/// ABI-decodes a value.
pub fn decode<T>(bytes: &[u8]) -> Result<T, DecodeError>
where
    T: Decode,
{
    let mut decoder = Decoder::new(bytes);

    // Make sure to call `decode` on the type instead of `read`. This is
    // because the top level tuple that gets encoded never gets redirected
    // even if it is a dynamic type.
    T::decode(&mut decoder)
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

    /// Reads a `usize` with overflow checking.
    pub fn read_size(&mut self) -> Result<usize, DecodeError> {
        usize::try_from(self.read::<U256>()?).map_err(|_| DecodeError::SizeOverflow)
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
    pub fn slice(&mut self) -> Result<Self, DecodeError> {
        let offset = self.read_size()?;
        let bytes = &self.buffer.get(offset..).ok_or(DecodeError::EndOfBuffer)?;
        Ok(Self::new(bytes))
    }

    /// Returns an decoder anchored at the current position.
    pub fn anchor(&self) -> Self {
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
        if T::is_dynamic() {
            T::decode(&mut self.slice()?)
        } else {
            T::decode(self)
        }
    }
}

/// An error during decoding.
#[derive(Debug)]
pub enum DecodeError {
    /// The end of buffer was reached while still decoding.
    EndOfBuffer,
    /// A was specified which overflows a `usize`.
    ///
    /// This can happen when decoding memory offsets or array lengths.
    SizeOverflow,
    /// Some invalid data was encountered.
    ///
    /// This can happen, for example, if a non-UTF-8 character was found when
    /// decoding a string. This can also be used by user-defined types to
    /// indicate that the data encountered when decoding is invalid.
    InvalidData,
}

impl Display for DecodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DecodeError::EndOfBuffer => f.write_str("unexpected end of buffer while decoding"),
            DecodeError::SizeOverflow => f.write_str("value overflows a `usize`"),
            DecodeError::InvalidData => f.write_str("invalid data while decoding"),
        }
    }
}

impl Error for DecodeError {}

impl<T> Decode for T
where
    T: Primitive,
{
    fn is_dynamic() -> bool {
        false
    }

    fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError> {
        Ok(T::cast(decoder.read_word()?))
    }
}

impl<T, const N: usize> Decode for [T; N]
where
    T: Decode,
{
    fn is_dynamic() -> bool {
        T::is_dynamic()
    }

    fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError> {
        // SAFETY: Copy of the unstable standard library implementation of
        // `MaybeUninit::uninit_array()`.
        let mut result: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };

        for element in result.iter_mut() {
            element.write(decoder.read()?);
        }

        // TODO(nlordell): Call drop when encountering an error while decoding
        // for values that were successfully decoded. Otherwise, if decoding
        // something like `uint256[][2]` (i.e. fixed array of dynamic arrays),
        // we will leak memory if the first dynamic array is read correctly and
        // the second one fails to read. WRITE A TEST FOR THIS.

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
    fn is_dynamic() -> bool {
        true
    }

    fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError> {
        let len = decoder.read_size()?;
        let mut result = Vec::with_capacity(len);
        let mut decoder = decoder.anchor();
        for _ in 0..len {
            result.push(decoder.read()?);
        }
        Ok(result)
    }
}

impl Decode for String {
    fn is_dynamic() -> bool {
        true
    }

    fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError> {
        let len = decoder.read_size()?;
        Ok(str::from_utf8(decoder.read_bytes(len)?)
            .map_err(|_| DecodeError::InvalidData)?
            .to_owned())
    }
}

macro_rules! impl_decode_for_tuple {
    ($($t:ident),*) => {
        #[allow(non_snake_case, unused_variables)]
        impl<$($t),*> Decode for ($($t,)*)
        where
            $($t: Decode,)*
        {
            fn is_dynamic() -> bool {
                false $(|| $t::is_dynamic())*
            }

            fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError> {
                $(let $t = decoder.read()?;)*
                Ok(($($t,)*))
            }
        }
    };
}

impl_decode_for_tuple! {}
impl_decode_for_tuple! { A }
impl_decode_for_tuple! { A, B }
impl_decode_for_tuple! { A, B, C }
impl_decode_for_tuple! { A, B, C, D }
impl_decode_for_tuple! { A, B, C, D, E }
impl_decode_for_tuple! { A, B, C, D, E, F }
impl_decode_for_tuple! { A, B, C, D, E, F, G }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD, AE }
impl_decode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD, AE, AF }
