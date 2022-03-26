//! Module implementing ABI encoding.

use crate::types::{bytes::Bytes, Primitive, Word};
use std::mem;

/// Represents an encodable type.
pub trait Encode {
    /// Returns the size information for the type.
    fn size(&self) -> Size;

    /// Writes the type's data to the specified encoder.
    fn encode(&self, encoder: &mut Encoder);
}

/// ABI-encodes a value.
pub fn encode<T>(value: T) -> Vec<u8>
where
    T: Encode,
{
    let size = value.size();
    let mut buffer = vec![0; size.byte_length()];
    let mut encoder = Encoder::new(&mut buffer, size);

    // Make sure to call `encode` on the value instead of `write`. This is
    // because the top level tuple that gets encoded never gets redirected
    // even if it is a dynamic type.
    value.encode(&mut encoder);

    buffer
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
        assert_eq!(
            buffer.len(),
            size.byte_length(),
            "buffer length does not match encoder size"
        );

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
    fn size(&self) -> Size {
        (**self).size()
    }

    fn encode(&self, encoder: &mut Encoder) {
        (**self).encode(encoder)
    }
}

impl<T> Encode for &'_ [T]
where
    T: Encode,
{
    fn size(&self) -> Size {
        let tail = Size::tuple(self.iter().map(|item| item.size())).total_word_count();
        Size::Dynamic(1, tail)
    }

    fn encode(&self, encoder: &mut Encoder) {
        encoder.write(&self.len());

        let inner_size = Size::tuple(self.iter().map(|item| item.size()));
        let mut inner = encoder.untail(inner_size);
        for item in *self {
            inner.write(item)
        }
    }
}

impl<T> Encode for Vec<T>
where
    T: Encode,
{
    fn size(&self) -> Size {
        (&**self).size()
    }

    fn encode(&self, encoder: &mut Encoder) {
        (&**self).encode(encoder)
    }
}

impl Encode for &'_ str {
    fn size(&self) -> Size {
        Bytes(self.as_bytes()).size()
    }

    fn encode(&self, encoder: &mut Encoder) {
        Bytes(self.as_bytes()).encode(encoder)
    }
}

impl Encode for String {
    fn size(&self) -> Size {
        (&**self).size()
    }

    fn encode(&self, encoder: &mut Encoder) {
        (&**self).encode(encoder)
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
            fn size(&self) -> Size {
                (**self).size()
            }

            fn encode(&self, encoder: &mut Encoder) {
                (**self).encode(encoder)
            }
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
    use crate::types::function::{FunctionPtr, Selector};
    use hex_literal::hex;

    #[test]
    fn ethereum_basic_abi_tests() {
        // <https://github.com/ethereum/tests/blob/0e8d25bb613cab7f9e99430f970e1e6cbffdbf1a/ABITests/basic_abi_tests.json>

        assert_eq!(
            encode((
                291,
                &[1110, 1929][..],
                Bytes(*b"1234567890"),
                "Hello, world!"
            )),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000123
                 0000000000000000000000000000000000000000000000000000000000000080
                 3132333435363738393000000000000000000000000000000000000000000000
                 00000000000000000000000000000000000000000000000000000000000000e0
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000456
                 0000000000000000000000000000000000000000000000000000000000000789
                 000000000000000000000000000000000000000000000000000000000000000d
                 48656c6c6f2c20776f726c642100000000000000000000000000000000000000"
            ),
        );

        assert_eq!(
            encode(98127491),
            hex!("0000000000000000000000000000000000000000000000000000000005d94e83"),
        );

        assert_eq!(
            encode((324124, addr!("CD2a3d9F938E13CD947Ec05AbC7FE734Df8DD826"))),
            hex!(
                "000000000000000000000000000000000000000000000000000000000004f21c
                 000000000000000000000000cd2a3d9f938e13cd947ec05abc7fe734df8dd826"
            ),
        );
    }

    #[test]
    fn solidity_abi_encoder_tests() {
        // <https://github.com/ethereum/solidity/blob/43f29c00da02e19ff10d43f7eb6955d627c57728/test/libsolidity/ABIEncoderTests.cpp>

        assert_eq!(
            encode((
                10,
                u16::MAX - 1,
                0x121212,
                -1,
                Bytes(hex!("1babab")),
                true,
                addr!("fffffffffffffffffffffffffffffffffffffffb")
            )),
            hex!(
                "000000000000000000000000000000000000000000000000000000000000000a
                 000000000000000000000000000000000000000000000000000000000000fffe
                 0000000000000000000000000000000000000000000000000000000000121212
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 1babab0000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000001
                 000000000000000000000000fffffffffffffffffffffffffffffffffffffffb"
            ),
        );

        assert_eq!(
            encode((
                "abcdef",
                Bytes(*b"abcde\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"),
                "abcdefabcdefgehabcabcasdfjklabcdefabcedefghabcabcasdfjklabcdefab\
                 cdefghabcabcasdfjklabcdeefabcdefghabcabcasdefjklabcdefabcdefghab\
                 cabcasdfjkl",
            )),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000060
                 6162636465000000000000000000000000000000000000000000000000000000
                 00000000000000000000000000000000000000000000000000000000000000a0
                 0000000000000000000000000000000000000000000000000000000000000006
                 6162636465660000000000000000000000000000000000000000000000000000
                 000000000000000000000000000000000000000000000000000000000000008b
                 616263646566616263646566676568616263616263617364666a6b6c61626364
                 6566616263656465666768616263616263617364666a6b6c6162636465666162
                 636465666768616263616263617364666a6b6c61626364656566616263646566
                 676861626361626361736465666a6b6c61626364656661626364656667686162
                 63616263617364666a6b6c000000000000000000000000000000000000000000"
            ),
        );

        assert_eq!(
            encode(0_u8),
            hex!("0000000000000000000000000000000000000000000000000000000000000000"),
        );
        assert_eq!(
            encode(1_u8),
            hex!("0000000000000000000000000000000000000000000000000000000000000001"),
        );
        assert_eq!(
            encode(2_u8),
            hex!("0000000000000000000000000000000000000000000000000000000000000002"),
        );

        assert_eq!(
            encode((
                Bytes([0_u8, 0, 0, 10]),
                Bytes(hex!("f1f20000")),
                0xff,
                0xff,
                -1,
                1,
            )),
            hex!(
                "0000000a00000000000000000000000000000000000000000000000000000000
                 f1f2000000000000000000000000000000000000000000000000000000000000
                 00000000000000000000000000000000000000000000000000000000000000ff
                 00000000000000000000000000000000000000000000000000000000000000ff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 0000000000000000000000000000000000000000000000000000000000000001"
            ),
        );

        assert_eq!(
            encode((10, vec![0xfffffffe_u64, 0xffffffff, 0x100000000], 11)),
            hex!(
                "000000000000000000000000000000000000000000000000000000000000000a
                 0000000000000000000000000000000000000000000000000000000000000060
                 000000000000000000000000000000000000000000000000000000000000000b
                 0000000000000000000000000000000000000000000000000000000000000003
                 00000000000000000000000000000000000000000000000000000000fffffffe
                 00000000000000000000000000000000000000000000000000000000ffffffff
                 0000000000000000000000000000000000000000000000000000000100000000"
            ),
        );

        assert_eq!(
            encode((10, [vec![7_i16, 0x0506, -1], vec![4, 5]], 11)),
            hex!(
                "000000000000000000000000000000000000000000000000000000000000000a
                 0000000000000000000000000000000000000000000000000000000000000060
                 000000000000000000000000000000000000000000000000000000000000000b
                 0000000000000000000000000000000000000000000000000000000000000040
                 00000000000000000000000000000000000000000000000000000000000000c0
                 0000000000000000000000000000000000000000000000000000000000000003
                 0000000000000000000000000000000000000000000000000000000000000007
                 0000000000000000000000000000000000000000000000000000000000000506
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000004
                 0000000000000000000000000000000000000000000000000000000000000005"
            ),
        );

        assert_eq!(
            encode((
                10,
                vec![
                    "abcabcdefghjklmnopqrsuvwabcdefgijklmnopqrstuwabcdefgijklmnoprstuvw",
                    "abcdefghijklmnopqrtuvwabcfghijklmnopqstuvwabcdeghijklmopqrstuvw",
                ],
                11
            )),
            hex!(
                "000000000000000000000000000000000000000000000000000000000000000a
                 0000000000000000000000000000000000000000000000000000000000000060
                 000000000000000000000000000000000000000000000000000000000000000b
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000040
                 00000000000000000000000000000000000000000000000000000000000000c0
                 0000000000000000000000000000000000000000000000000000000000000042
                 61626361626364656667686a6b6c6d6e6f707172737576776162636465666769
                 6a6b6c6d6e6f7071727374757761626364656667696a6b6c6d6e6f7072737475
                 7677000000000000000000000000000000000000000000000000000000000000
                 000000000000000000000000000000000000000000000000000000000000003f
                 6162636465666768696a6b6c6d6e6f70717274757677616263666768696a6b6c
                 6d6e6f7071737475767761626364656768696a6b6c6d6f707172737475767700"
            ),
        );

        assert_eq!(
            encode((
                "123456789012345678901234567890a",
                "ffff123456789012345678901234567890afffffffff123456789012345678901234567890a",
            )),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000080
                 000000000000000000000000000000000000000000000000000000000000001f
                 3132333435363738393031323334353637383930313233343536373839306100
                 000000000000000000000000000000000000000000000000000000000000004b
                 6666666631323334353637383930313233343536373839303132333435363738
                 3930616666666666666666663132333435363738393031323334353637383930
                 3132333435363738393061000000000000000000000000000000000000000000"
            ),
        );

        assert_eq!(
            encode([
                addr!("ffffffffffffffffffffffffffffffffffffffff"),
                addr!("fffffffffffffffffffffffffffffffffffffffe"),
                addr!("fffffffffffffffffffffffffffffffffffffffd"),
            ]),
            hex!(
                "000000000000000000000000ffffffffffffffffffffffffffffffffffffffff
                 000000000000000000000000fffffffffffffffffffffffffffffffffffffffe
                 000000000000000000000000fffffffffffffffffffffffffffffffffffffffd"
            ),
        );

        assert_eq!(
            encode((vec![
                addr!("0000000000000000000000000000000000000001"),
                addr!("0000000000000000000000000000000000000002"),
                addr!("0000000000000000000000000000000000000003"),
            ],)),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000020
                 0000000000000000000000000000000000000000000000000000000000000003
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000003"
            ),
        );

        assert_eq!(
            encode((vec![-1, 2, -3, 4, -5, 6, -7, 8,],)),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000020
                 0000000000000000000000000000000000000000000000000000000000000008
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 0000000000000000000000000000000000000000000000000000000000000002
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd
                 0000000000000000000000000000000000000000000000000000000000000004
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffb
                 0000000000000000000000000000000000000000000000000000000000000006
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff9
                 0000000000000000000000000000000000000000000000000000000000000008"
            ),
        );

        assert_eq!(
            encode((
                FunctionPtr {
                    address: addr!("001c08ab857afe5a9633887e7a4e2a429d1d8d42"),
                    selector: Selector(hex!("b3de648b")),
                },
                FunctionPtr {
                    address: addr!("001c08ab857afe5a9633887e7a4e2a429d1d8d42"),
                    selector: Selector(hex!("b3de648b")),
                },
            )),
            hex!(
                "001c08ab857afe5a9633887e7a4e2a429d1d8d42b3de648b0000000000000000
                 001c08ab857afe5a9633887e7a4e2a429d1d8d42b3de648b0000000000000000"
            ),
        );

        assert_eq!(
            encode((
                FunctionPtr {
                    address: addr!("ffffffffffffffffffffffffffffffffffffffff"),
                    selector: Selector(hex!("ffffffff")),
                },
                FunctionPtr {
                    address: addr!("ffffffffffffffffffffffffffffffffffffffff"),
                    selector: Selector(hex!("ffffffff")),
                },
            )),
            hex!(
                "ffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000
                 ffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000"
            ),
        );

        assert_eq!(
            encode(("abcdef",)),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000020
                 0000000000000000000000000000000000000000000000000000000000000006
                 6162636465660000000000000000000000000000000000000000000000000000"
            ),
        );
        assert_eq!(
            encode((
                "abcdefgggggggggggggggggggggggggggggggggggggggghhheeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
            )),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000020
                 000000000000000000000000000000000000000000000000000000000000004f
                 6162636465666767676767676767676767676767676767676767676767676767
                 6767676767676767676767676767686868656565656565656565656565656565
                 6565656565656565656565656565650000000000000000000000000000000000"
            ),
        );

        assert_eq!(
            encode(0),
            hex!("0000000000000000000000000000000000000000000000000000000000000000"),
        );
        assert_eq!(
            encode(1),
            hex!("0000000000000000000000000000000000000000000000000000000000000001"),
        );
        assert_eq!(
            encode(7),
            hex!("0000000000000000000000000000000000000000000000000000000000000007"),
        );

        assert_eq!(
            encode((7, (8, 9, vec![(11, 0), (12, 0), (0, 13),], 10))),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000007
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000008
                 0000000000000000000000000000000000000000000000000000000000000009
                 0000000000000000000000000000000000000000000000000000000000000080
                 000000000000000000000000000000000000000000000000000000000000000a
                 0000000000000000000000000000000000000000000000000000000000000003
                 000000000000000000000000000000000000000000000000000000000000000b
                 0000000000000000000000000000000000000000000000000000000000000000
                 000000000000000000000000000000000000000000000000000000000000000c
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000
                 000000000000000000000000000000000000000000000000000000000000000d"
            ),
        );

        assert_eq!(
            encode((
                7,
                [
                    (
                        addr!("1111111111111111111111111111111111111111"),
                        vec![(0x11, 1, 0x12)],
                    ),
                    Default::default(),
                ],
                vec![
                    Default::default(),
                    (
                        addr!("0000000000000000000000000000000000001234"),
                        vec![(0, 0, 0), (0x21, 2, 0x22), (0, 0, 0)]
                    ),
                ],
                8,
            )),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000007
                 0000000000000000000000000000000000000000000000000000000000000080
                 00000000000000000000000000000000000000000000000000000000000001e0
                 0000000000000000000000000000000000000000000000000000000000000008
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000100
                 0000000000000000000000001111111111111111111111111111111111111111
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000011
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000012
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000040
                 00000000000000000000000000000000000000000000000000000000000000a0
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000001234
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000003
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000021
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000022
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000"
            ),
        );

        assert_eq!(encode(()), hex!(""));
        assert_eq!(
            encode((vec![true, false, true, false], [true, false, true, false])),
            hex!(
                "00000000000000000000000000000000000000000000000000000000000000a0
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000004
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000000"
            ),
        );

        macro_rules! bytes_nn_arrays {
            ([$($size:literal),*] { $($nn:literal),* }) => {
                bytes_nn_arrays!(__outer: [ $($size),* ] { $($nn,)* });
            };
            (__impl: $size:literal $nn:literal) => {{
                const SIZE: usize = $size;
                const NN: usize = $nn;

                let mut y: [Bytes<[u8; NN]>; SIZE] = Default::default();
                for (i, y) in y.iter_mut().enumerate() {
                    y.0[NN - 1] = (i as u8) + 1;
                }

                let mut buffer = Vec::new();
                buffer.extend_from_slice(&(0x20 * (1 + SIZE)).to_word());
                for y in &y {
                    buffer.extend_from_slice(&y.to_word());
                }
                buffer.extend_from_slice(&2.to_word());
                buffer.extend_from_slice(&Bytes(*b"abc").to_word());
                buffer.extend_from_slice(&Bytes(*b"def").to_word());

                assert_eq!(encode((vec![Bytes(*b"abc"), Bytes(*b"def")], y)), buffer);
            }};

            // Cartesian product of `$t * $n`
            (__outer: [ $($size:literal),* ] $rest:tt) => {$(
                bytes_nn_arrays!(__inner: $size $rest);
            )*};
            (__inner: $size:literal { $($nn:literal,)* }) => {$(
                bytes_nn_arrays!(__impl: $size $nn);
            )*};
        }

        bytes_nn_arrays!(
            [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
            {1, 2, 4, 5, 7, 15, 16, 17, 31, 32}
        );

        macro_rules! bytes_nn_arrays_dyn {
            ([ $size:expr ] { $($nn:literal),* }) => {$({
                let size = $size;
                const NN: usize = $nn;

                for size in size {
                    let y = (0..size)
                        .map(|i| {
                            let mut e = [0_u8; NN];
                            e[NN - 1] = (i as u8) + 1;
                            Bytes(e)
                        })
                        .collect::<Vec<_>>();

                    let mut buffer = Vec::new();
                    buffer.extend_from_slice(&(0x20 * 2).to_word());
                    buffer.extend_from_slice(&(0x20 * (3 + size)).to_word());
                    buffer.extend_from_slice(&size.to_word());
                    for y in &y {
                        buffer.extend_from_slice(&y.to_word());
                    }
                    buffer.extend_from_slice(&2.to_word());
                    buffer.extend_from_slice(&Bytes(*b"abc").to_word());
                    buffer.extend_from_slice(&Bytes(*b"def").to_word());

                    assert_eq!(encode((&y[..], vec![Bytes(*b"abc"), Bytes(*b"def")])), buffer);
                }
            })*};
        }

        bytes_nn_arrays_dyn!([1..15] {1, 2, 4, 5, 7, 15, 16, 17, 31, 32});

        assert_eq!(
            encode((
                false,
                -5,
                FunctionPtr {
                    address: addr!("903d3a9a4266eb4432407ea5b1b4f80094f17957"),
                    selector: Selector(hex!("e2179b8e")),
                },
                Bytes(hex!("010203")),
                -3,
            )),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000000
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffb
                 903d3a9a4266eb4432407ea5b1b4f80094f17957e2179b8e0000000000000000
                 0102030000000000000000000000000000000000000000000000000000000000
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd"
            ),
        );

        assert_eq!(
            encode(("", 3, "")),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000060
                 0000000000000000000000000000000000000000000000000000000000000003
                 0000000000000000000000000000000000000000000000000000000000000080
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000"
            ),
        );

        assert_eq!(
            encode(("abc", 7, "def")),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000060
                 0000000000000000000000000000000000000000000000000000000000000007
                 00000000000000000000000000000000000000000000000000000000000000a0
                 0000000000000000000000000000000000000000000000000000000000000003
                 6162630000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000003
                 6465660000000000000000000000000000000000000000000000000000000000"
            ),
        );
    }
}
