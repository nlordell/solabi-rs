//! Solidity primitive integer types.

use super::{Primitive, Word};
use ethnum::{AsU256, I256, U256};
use std::ops::{Deref, Shl, Shr};

macro_rules! impl_primitive_for_as_u256 {
    ($($t:ty,)*) => {$(
        impl Primitive for $t {
            fn to_word(&self) -> Word {
                self.as_u256().to_be_bytes()
            }
        }
    )*};
}

impl_primitive_for_as_u256! {
    i8, i16, i32, i64, i128, I256, isize,
    u8, u16, u32, u64, u128, U256, usize,
    bool,
}

/// An signed integer with the specified width.
///
/// This type is needed because there aren't Rust equivalents for all Solidity
/// integer types (`int96` for example).
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Int<const N: usize>(I256);

impl<const N: usize> Int<N> {
    /// Creates a new signed integer.
    ///
    /// # Panics
    ///
    /// Panics on invalid bit-width.
    pub fn new(value: I256) -> Option<Self> {
        resize::<N, _>(value).map(Self)
    }
}

impl<const N: usize> Deref for Int<N> {
    type Target = I256;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// An unsigned integer with the specified width.
///
/// This type is needed because there aren't Rust equivalents for all Solidity
/// integer types (`uint96` for example).
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Uint<const N: usize>(U256);

impl<const N: usize> Uint<N> {
    /// Creates a new unsigned integer.
    ///
    /// # Panics
    ///
    /// Panics on invalid bit-width.
    pub fn new(value: U256) -> Option<Self> {
        resize::<N, _>(value).map(Self)
    }
}

impl<const N: usize> Deref for Uint<N> {
    type Target = U256;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Resizes an integer to fit in `N` bits.
///
/// # Panics
///
/// Panics on invalid bit-width.
fn resize<const N: usize, T>(value: T) -> Option<T>
where
    T: Copy + Eq + Shl<usize, Output = T> + Shr<usize, Output = T>,
{
    assert!(
        N % 8 == 0 && (1..=256).contains(&N),
        "invalid bit-width {N}"
    );
    let shift = 256 - N;
    let masked = value << shift >> shift;
    if masked == value {
        Some(value)
    } else {
        None
    }
}

macro_rules! impl_primitive_for_integer {
    ($($t:ident),* { $($n:literal,)* }) => {
        impl_primitive_for_integer!(__outer: { $($t),* } { $($n,)* });
    };
    (__impl: $t:ident $n:literal) => {
        impl Primitive for $t<$n> {
            fn to_word(&self) -> Word {
                self.0.to_be_bytes()
            }
        }
    };

    // Cartesian product of `$t * $n`
    (__outer: { $($t:ident),* } $rest:tt) => {$(
        impl_primitive_for_integer!(__inner: $t $rest);
    )*};
    (__inner: $t:ident { $($n:literal,)* }) => {$(
        impl_primitive_for_integer!(__impl: $t $n);
    )*};
}

impl_primitive_for_integer! {
    Int, Uint {
          8,  16,  24,  32,  40,  48,  56,  64,
         72,  80,  88,  96, 104, 112, 120, 128,
        136, 144, 152, 160, 168, 176, 184, 192,
        200, 208, 216, 224, 232, 240, 248, 256,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sign_extends_integers() {
        assert_eq!((1_i32).to_word(), {
            let mut bytes = [0; 32];
            bytes[31] = 1;
            bytes
        });
        assert_eq!((-1_i32).to_word(), [0xff; 32]);
    }

    #[test]
    fn implements_primitive() {
        fn assert_implements_primitve<T: Primitive>() {}

        assert_implements_primitve::<Int<8>>();
        assert_implements_primitve::<Int<16>>();
        assert_implements_primitve::<Int<24>>();
        assert_implements_primitve::<Uint<8>>();
        assert_implements_primitve::<Uint<16>>();
        assert_implements_primitve::<Uint<24>>();
    }
}
