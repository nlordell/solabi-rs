//! Solidity primitive type trait and implementations.

use ethaddr::Address;
use ethnum::{AsI256 as _, I256, U256};

/// An Ethereum 32-byte word.
pub type Word = [u8; 32];

/// Trait reprenting any type that can be converted to and from a single
/// Ethereum 32-byte word.
pub trait Primitive {
    /// Converts a primitive type to an Ethereum 32-byte word.
    fn to_word(&self) -> Word;

    /// Converts an Ethereum 32-byte word as the type.
    ///
    /// Note that this should implement casting semantics. In other words a word
    /// doesn't necessarily round trip as some bytes may be truncated during
    /// conversion. For example, `0xfff..fff` converted to a `uint8` will result
    /// in `0xff` and when converted back to a word would yield `0x000..0ff`.
    fn from_word(word: Word) -> Self;
}

macro_rules! impl_primitive_for_i256 {
    ($($t:ty,)*) => {$(
        impl Primitive for $t {
            fn to_word(&self) -> Word {
                self.to_be_bytes()
            }

            fn from_word(word: Word) -> Self {
                Self::from_be_bytes(word)
            }
        }
    )*};
}

impl_primitive_for_i256! {
    I256,
    U256,
}

macro_rules! impl_primitive_for_integer {
    ($($t:ty,)*) => {$(
        impl Primitive for $t {
            fn to_word(&self) -> Word {
                self.as_i256().to_word()
            }

            fn from_word(word: Word) -> Self {
                *I256::from_word(word).low() as _
            }
        }
    )*};
}

impl_primitive_for_integer! {
    i8, i16, i32, i64, i128, isize,
    u8, u16, u32, u64, u128, usize,
}

impl Primitive for bool {
    fn to_word(&self) -> Word {
        [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, *self as _,
        ]
    }

    fn from_word(word: Word) -> Self {
        word != [0; 32]
    }
}

impl Primitive for Address {
    fn to_word(&self) -> Word {
        let mut word = Word::default();
        word[12..].copy_from_slice(&**self);
        word
    }

    fn from_word(word: Word) -> Self {
        Self::from_slice(&word[12..])
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
}
