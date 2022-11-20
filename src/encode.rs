//! Module implementing ABI encoding.

use crate::{
    bytes::Bytes,
    function::Selector,
    primitive::{Primitive, Word},
};
use std::{
    borrow::Cow,
    error::Error,
    fmt::{self, Display, Formatter},
    mem,
};

/// Represents an encodable type.
pub trait Encode {
    /// Returns the size information for the type.
    fn size(&self) -> Size;

    /// Writes the type's data to the specified encoder.
    ///
    /// # Notes
    ///
    /// Encoding values that do not match what is returned by [`Encode::size`]
    /// may cause the encoding to panic.
    fn encode(&self, encoder: &mut Encoder);
}

/// ABI-encodes a value.
pub fn encode<T>(value: &T) -> Vec<u8>
where
    T: Encode,
{
    let mut buffer = vec![0; value.size().byte_length()];
    encode_to(&mut buffer, value).unwrap();
    buffer
}

/// ABI-encodes a value with a selector.
pub fn encode_with_selector<T>(selector: Selector, value: &T) -> Vec<u8>
where
    T: Encode,
{
    let sel = selector.as_ref();

    let mut buffer = vec![0; value.size().byte_length() + sel.len()];
    buffer[..sel.len()].copy_from_slice(sel);

    encode_to(&mut buffer[sel.len()..], value).unwrap();
    buffer
}

/// ABI-encodes a value with a byte prefix.
pub fn encode_with_prefix<T>(prefix: &[u8], value: &T) -> Vec<u8>
where
    T: Encode,
{
    let mut buffer = vec![0; value.size().byte_length() + prefix.len()];
    buffer[..prefix.len()].copy_from_slice(prefix);
    encode_to(&mut buffer[prefix.len()..], value).unwrap();
    buffer
}

/// ABI-encodes a value to the specified buffer.
pub fn encode_to<T>(buffer: &mut [u8], value: &T) -> Result<(), BufferSizeError>
where
    T: Encode,
{
    let size = value.size();
    if buffer.len() != size.byte_length() {
        return Err(BufferSizeError);
    }

    let mut encoder = Encoder::new(buffer, size);

    // Make sure to call `encode` on the value instead of `write`. This is
    // because the top level tuple that gets encoded never gets redirected
    // even if it is a dynamic type.
    value.encode(&mut encoder);

    Ok(())
}

/// Encoding size.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Size {
    /// Static type size, specifying the number of words required to represent
    /// the type.
    Static(usize),

    /// Dynamic type size, specifying the number of words required to represent
    /// the "head" and the "tail" of the type.
    Dynamic(usize, usize),
}

impl Size {
    /// Combines multiple sizes of fields into the size of their tuple.
    pub fn tuple(fields: impl IntoIterator<Item = Size>) -> Self {
        fields
            .into_iter()
            .fold(Self::Static(0), |acc, size| match (acc, size) {
                (Self::Static(h0), Self::Static(h1)) => Self::Static(h0 + h1),
                (Self::Static(h0), Self::Dynamic(h1, t1)) => Self::Dynamic(h0 + 1, h1 + t1),
                (Self::Dynamic(h0, t0), Self::Static(h1)) => Self::Dynamic(h0 + h1, t0),
                _ => {
                    let (h0, t0) = acc.word_count();
                    let (h1, t1) = size.word_count();
                    Self::Dynamic(h0 + 1, t0 + h1 + t1)
                }
            })
    }

    /// Combines sizes of elements of a dynamic array.
    pub fn dynamic_array(elements: impl IntoIterator<Item = Size>) -> Self {
        let tail = Self::tuple(elements).total_word_count();
        Self::Dynamic(1, tail)
    }

    /// Returns the head and tail word counts required for the spcified size.
    ///
    /// Note that for static types, the tail word count is always 0.
    pub fn word_count(&self) -> (usize, usize) {
        match self {
            Self::Static(head) => (*head, 0),
            Self::Dynamic(head, tail) => (*head, *tail),
        }
    }

    /// Returns the total word count of the head and tail combined.
    pub fn total_word_count(&self) -> usize {
        let (head, tail) = self.word_count();
        head + tail
    }

    /// Returns the byte-length for the specified size.
    pub fn byte_length(&self) -> usize {
        self.total_word_count() * 32
    }

    /// Returns the offset, in bytes, of the tail.
    pub fn tail_byte_offset(&self) -> usize {
        let (head, _) = self.word_count();
        head * 32
    }

    /// Returns `true` if the type is static.
    pub fn is_static(&self) -> bool {
        match self {
            Self::Static(..) => true,
            Self::Dynamic(..) => false,
        }
    }

    /// Returns `true` if the type is dynamic.
    pub fn is_dynamic(&self) -> bool {
        match self {
            Self::Static(..) => false,
            Self::Dynamic(..) => true,
        }
    }
}

/// An ABI encoder
pub struct Encoder<'a> {
    head: &'a mut [u8],
    tail: &'a mut [u8],
    tail_offset: usize,
}

impl<'a> Encoder<'a> {
    /// Create a new ABI encoder with the specified buffer and head/tail word
    /// count.
    ///
    /// # Panics
    ///
    /// Panics if the buffer size does not match the word count.
    fn new(buffer: &'a mut [u8], size: Size) -> Self {
        debug_assert_eq!(buffer.len(), size.byte_length(), "{BufferSizeError}");

        let tail_offset = size.tail_byte_offset();
        let (head, tail) = buffer.split_at_mut(tail_offset);
        Self {
            head,
            tail,
            tail_offset,
        }
    }

    /// Writes a word to the encoder.
    pub fn write_word(&mut self, word: Word) {
        self.write_bytes(&word);
    }

    /// Writes a slice of bytes to the encoder.
    pub fn write_bytes(&mut self, bytes: &[u8]) {
        let slot = take(&mut self.head, pad32(bytes.len()));
        slot[..bytes.len()].copy_from_slice(bytes);
    }

    /// Slices a chunk off of the encoder's tail.
    ///
    /// This method is used for getting a sub-`Encoder` for writing the contents
    /// of a dynamic type.
    pub fn slice(&mut self, size: Size) -> Self {
        let offset = self.tail_offset;
        self.write(&offset);

        let len = size.byte_length();
        let slot = take(&mut self.tail, len);
        self.tail_offset += len;

        Self::new(slot, size)
    }

    /// Returns an encoder for the remaining tail.
    pub fn untail(&mut self, size: Size) -> Self {
        let len = size.byte_length();
        let slot = take(&mut self.tail, len);

        Self::new(slot, size)
    }

    /// Writes a value to the encoder.
    ///
    /// This method takes care to either encode the value directly for static
    /// types or slice off some section of the "tail" for dynamic types.
    pub fn write<T>(&mut self, value: &T)
    where
        T: Encode,
    {
        match value.size() {
            Size::Static(..) => value.encode(self),
            size => value.encode(&mut self.slice(size)),
        }
    }
}

/// An error indicating that a buffer of incorrect size was used.
#[derive(Debug)]
pub struct BufferSizeError;

impl Display for BufferSizeError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("buffer length does not match encoder size")
    }
}

impl Error for BufferSizeError {}

/// Pads the specified size to a 32-byte boundry.
fn pad32(value: usize) -> usize {
    ((value + 31) / 32) * 32
}

/// Splits an array in-place returning a mutable slice to the chunk that was
/// split of the front.
fn take<'a>(buffer: &mut &'a mut [u8], len: usize) -> &'a mut [u8] {
    let (slot, rest) = mem::take(buffer).split_at_mut(len);
    *buffer = rest;
    slot
}

impl<T> Encode for T
where
    T: Primitive,
{
    fn size(&self) -> Size {
        Size::Static(1)
    }

    fn encode(&self, encoder: &mut Encoder) {
        encoder.write_word(self.to_word())
    }
}

macro_rules! impl_encode_for_ref {
    () => {
        fn size(&self) -> Size {
            (**self).size()
        }

        fn encode(&self, encoder: &mut Encoder) {
            (**self).encode(encoder)
        }
    };
}

impl<T, const N: usize> Encode for [T; N]
where
    T: Encode,
{
    fn size(&self) -> Size {
        Size::tuple(self.iter().map(|item| item.size()))
    }

    fn encode(&self, encoder: &mut Encoder) {
        for item in self {
            encoder.write(item)
        }
    }
}

impl<T, const N: usize> Encode for &'_ [T; N]
where
    T: Encode,
{
    impl_encode_for_ref!();
}

impl<T> Encode for [T]
where
    T: Encode,
{
    fn size(&self) -> Size {
        Size::dynamic_array(self.iter().map(|item| item.size()))
    }

    fn encode(&self, encoder: &mut Encoder) {
        encoder.write(&self.len());
        let inner_size = Size::tuple(self.iter().map(|item| item.size()));
        let mut inner = encoder.untail(inner_size);
        for item in self {
            inner.write(item)
        }
    }
}

impl<T> Encode for &'_ [T]
where
    T: Encode,
{
    impl_encode_for_ref!();
}

impl<T> Encode for Vec<T>
where
    T: Encode,
{
    fn size(&self) -> Size {
        (**self).size()
    }

    fn encode(&self, encoder: &mut Encoder) {
        (**self).encode(encoder)
    }
}

impl Encode for str {
    fn size(&self) -> Size {
        Bytes(self.as_bytes()).size()
    }

    fn encode(&self, encoder: &mut Encoder) {
        Bytes(self.as_bytes()).encode(encoder)
    }
}

impl Encode for &'_ str {
    impl_encode_for_ref!();
}

impl Encode for String {
    fn size(&self) -> Size {
        (**self).size()
    }

    fn encode(&self, encoder: &mut Encoder) {
        (**self).encode(encoder)
    }
}

impl<T> Encode for Cow<'_, T>
where
    T: Encode + ToOwned + ?Sized,
{
    fn size(&self) -> Size {
        self.as_ref().size()
    }

    fn encode(&self, encoder: &mut Encoder) {
        self.as_ref().encode(encoder)
    }
}

macro_rules! impl_encode_for_tuple {
    ($($t:ident),*) => {
        #[allow(non_snake_case, unused_variables)]
        impl<$($t),*> Encode for ($($t,)*)
        where
            $($t: Encode,)*
        {
            fn size(&self) -> Size {
                let ($($t,)*) = self;
                Size::tuple([
                    $(($t).size(),)*
                ])
            }

            fn encode(&self, encoder: &mut Encoder) {
                let ($($t,)*) = self;
                $(encoder.write($t);)*
            }
        }

        impl<$($t),*> Encode for &'_ ($($t,)*)
        where
            $($t: Encode,)*
        {
            impl_encode_for_ref!();
        }
    };
}

impl_encode_for_tuple! {}
impl_encode_for_tuple! { A }
impl_encode_for_tuple! { A, B }
impl_encode_for_tuple! { A, B, C }
impl_encode_for_tuple! { A, B, C, D }
impl_encode_for_tuple! { A, B, C, D, E }
impl_encode_for_tuple! { A, B, C, D, E, F }
impl_encode_for_tuple! { A, B, C, D, E, F, G }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD, AE }
impl_encode_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD, AE, AF }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_with_refs() {
        assert_eq!(encode(&([1, 2, 3])), encode(&(&[1, 2, 3])));
        assert_eq!(encode(&(vec![1, 2, 3])), encode(&([1, 2, 3].as_slice())));
        assert_eq!(encode(&((1, true), 2)), encode(&(&(1, true), 2)));
        assert_eq!(encode(&(String::from("hello"))), encode(&("hello")));
    }

    #[test]
    fn encode_with_borrowed_and_owned_cow() {
        let borrowed = Cow::Borrowed("moo");
        let owned = Cow::Owned("moo".to_owned());

        assert_eq!(encode::<Cow<str>>(&borrowed), encode::<Cow<str>>(&owned));
    }
}
