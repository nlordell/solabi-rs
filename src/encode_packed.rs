//! Module implementing packed encoding.

use ethprim::{Address, Digest, I256, U256};

use crate::encode::BufferSizeError;
use std::borrow::Cow;

/// Represents a packed-encodable type
pub trait EncodePacked {
    /// Returns the packed size for the type
    fn packed_size(&self) -> usize;

    /// Writes the type's data to the specified buffer.
    ///
    /// # Panics
    ///
    /// Encoding values that do not match what is returned by [`Encode::size`]
    /// may cause the encoding to panic.
    fn encode_packed(&self, out: &mut [u8]);
}

/// Packed-encodes a value.
pub fn encode_packed<T: EncodePacked>(value: &T) -> Vec<u8> {
    let mut buf = vec![0u8; value.packed_size()];
    encode_packed_to(&mut buf, value).unwrap();
    buf
}

/// Packed-encodes a value to the specified buffer.
pub fn encode_packed_to<T>(buffer: &mut [u8], value: &T) -> Result<(), BufferSizeError>
where
    T: EncodePacked,
{
    if buffer.len() != value.packed_size() {
        return Err(BufferSizeError);
    }

    value.encode_packed(buffer);
    Ok(())
}

macro_rules! impl_encode_packed_for_integer {
    ($($t:ty),* $(,)?) => {
        $(
            impl EncodePacked for $t {
                fn encode_packed(&self, out: &mut [u8]) {
                    out.copy_from_slice(&self.to_be_bytes());
                }

                fn packed_size(&self) -> usize {
                    std::mem::size_of::<Self>()
                }
            }
        )*
    };
}

impl_encode_packed_for_integer!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize,);

macro_rules! impl_encode_packed_for_ref {
    () => {
        fn packed_size(&self) -> usize {
            (**self).packed_size()
        }

        fn encode_packed(&self, out: &mut [u8]) {
            (**self).encode_packed(out)
        }
    };
}

impl<T> EncodePacked for [T]
where
    T: EncodePacked,
{
    fn packed_size(&self) -> usize {
        self.iter().map(|item| item.packed_size()).sum()
    }

    fn encode_packed(&self, out: &mut [u8]) {
        let mut offset = 0;
        for item in self {
            let end = offset + item.packed_size();
            item.encode_packed(&mut out[offset..end]);
            offset = end;
        }
    }
}

impl<T, const N: usize> EncodePacked for [T; N]
where
    T: EncodePacked,
{
    fn packed_size(&self) -> usize {
        self.as_slice().packed_size()
    }

    fn encode_packed(&self, out: &mut [u8]) {
        self.as_slice().encode_packed(out)
    }
}

impl<T, const N: usize> EncodePacked for &'_ [T; N]
where
    T: EncodePacked,
{
    impl_encode_packed_for_ref!();
}

impl<T> EncodePacked for Vec<T>
where
    T: EncodePacked,
{
    impl_encode_packed_for_ref!();
}

impl EncodePacked for str {
    fn packed_size(&self) -> usize {
        self.as_bytes().packed_size()
    }

    fn encode_packed(&self, out: &mut [u8]) {
        self.as_bytes().encode_packed(out)
    }
}

impl EncodePacked for &'_ str {
    impl_encode_packed_for_ref!();
}

impl EncodePacked for String {
    impl_encode_packed_for_ref!();
}

impl<T> EncodePacked for Cow<'_, T>
where
    T: EncodePacked + ToOwned + ?Sized,
{
    fn packed_size(&self) -> usize {
        self.as_ref().packed_size()
    }

    fn encode_packed(&self, out: &mut [u8]) {
        self.as_ref().encode_packed(out)
    }
}

macro_rules! impl_encode_packed_for_tuple {
    ($($t:ident),*) => {
        #[allow(non_snake_case, unused_variables)]
        impl<$($t),*> EncodePacked for ($($t,)*)
        where
            $($t: EncodePacked,)*
        {
            fn packed_size(&self) -> usize {
                let ($($t,)*) = self;
                0 $(+ $t.packed_size())*
            }

            #[allow(unused_assignments)]
            fn encode_packed(&self, out: &mut [u8]) {
                let ($($t,)*) = self;
                #[allow(unused_mut)]
                let mut offset = 0;

                $(

                    let end = offset + $t.packed_size();
                    $t.encode_packed(&mut out[offset..end]);
                    offset = end;
                )*
            }
        }

        impl<$($t),*> EncodePacked for &'_ ($($t,)*)
        where
            $($t: EncodePacked,)*
        {
            impl_encode_packed_for_ref!();
        }
    };
}

impl_encode_packed_for_tuple! {}
impl_encode_packed_for_tuple! { A }
impl_encode_packed_for_tuple! { A, B }
impl_encode_packed_for_tuple! { A, B, C }
impl_encode_packed_for_tuple! { A, B, C, D }
impl_encode_packed_for_tuple! { A, B, C, D, E }
impl_encode_packed_for_tuple! { A, B, C, D, E, F }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD, AE }
impl_encode_packed_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD, AE, AF }

macro_rules! impl_encode_packed_for_i256 {
    ($($t:ty,)*) => {$(
        impl EncodePacked for $t {
            fn packed_size(&self) -> usize {
                32
            }

            fn encode_packed(&self, out: &mut [u8]) {
                self.to_be_bytes().encode_packed(out)
            }
        }
    )*};
}

impl_encode_packed_for_i256! {
    I256,
    U256,
}

impl EncodePacked for bool {
    fn packed_size(&self) -> usize {
        1
    }

    fn encode_packed(&self, out: &mut [u8]) {
        out[0] = *self as u8;
    }
}

impl EncodePacked for Address {
    fn packed_size(&self) -> usize {
        20
    }

    fn encode_packed(&self, out: &mut [u8]) {
        out.copy_from_slice(&**self);
    }
}

impl EncodePacked for Digest {
    fn packed_size(&self) -> usize {
        32
    }

    fn encode_packed(&self, out: &mut [u8]) {
        out.copy_from_slice(&**self);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hex_literal::hex;

    #[test]
    fn encode_basic_types() {
        assert_eq!(encode_packed(&0u8), hex!("00"));
        assert_eq!(encode_packed(&255u8), hex!("ff"));
        assert_eq!(encode_packed(&0x1234u16), hex!("1234"));
        assert_eq!(encode_packed(&0x01020304i32), hex!("01020304"));
        assert_eq!(encode_packed(&-1i32), hex!("ffffffff"));
        let encoded = encode_packed(&1usize);
        // Generate hex literal of length based on usize
        let usize_result = {
            let mut bytes = [0_u8; std::mem::size_of::<usize>()];
            *bytes.last_mut().unwrap() = 1;
            bytes
        };
        assert!(encoded == usize_result);
        assert_eq!(encode_packed(&-128i8), hex!("80"));
        assert_eq!(encode_packed(&127i8), hex!("7f"));

        // Tests for arrays
        assert_eq!(encode_packed(&[1u8, 2, 3]), hex!("010203"));

        // Tests for strings and variants
        assert_eq!(encode_packed(&"hello"), hex!("68656c6c6f"));

        // Tests for tuples
        assert_eq!(encode_packed(&(1u8, 2u8)), hex!("0102"));
        assert_eq!(encode_packed(&(1u8, 2u8, 3u8)), hex!("010203"));
        assert_eq!(encode_packed(&(1u8, 2u16, 3u32)), hex!("01000200000003"));
    }
}
