//! Solidity primitive type trait and implementations.

use ethprim::{Address, AsI256 as _, Digest, I256, U256};

/// An Ethereum 32-byte word.
pub type Word = [u8; 32];

/// Trait reprenting any type that can be converted to and from a single
/// Ethereum 32-byte word.
pub trait Primitive: Sized {
    /// Converts a primitive type to an Ethereum 32-byte word.
    fn to_word(&self) -> Word;

    /// Converts an Ethereum 32-byte word as the type. Returns `None` if the
    /// word is incorrectly padded or sign-extended. In other words a word must
    /// round trip.
    ///
    /// # Examples
    ///
    /// ```
    /// # use solabi::primitive::Primitive as _;
    /// assert_eq!(i8::from_word([0; 32]), Some(0));
    /// assert_eq!(i8::from_word([0xff; 32]), Some(-1));
    /// assert_eq!(i8::from_word([0xfe; 32]), None);
    /// assert_eq!(u8::from_word([0; 32]), Some(0));
    /// assert_eq!(u8::from_word([0xff; 32]), None);
    /// ```
    fn from_word(word: Word) -> Option<Self>;
}

macro_rules! impl_primitive_for_i256 {
    ($($t:ty,)*) => {$(
        impl Primitive for $t {
            fn to_word(&self) -> Word {
                self.to_be_bytes()
            }

            fn from_word(word: Word) -> Option<Self> {
                Some(Self::from_be_bytes(word))
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

            fn from_word(word: Word) -> Option<Self> {
                #[allow(unused_comparisons)]
                const SIGNED: bool = <$t>::MIN < 0;
                if SIGNED {
                    I256::from_be_bytes(word).try_into().ok()
                } else {
                    U256::from_be_bytes(word).try_into().ok()
                }
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

    fn from_word(word: Word) -> Option<Self> {
        match U256::from_be_bytes(word).into_words() {
            (0, 0) => Some(false),
            (0, 1) => Some(true),
            _ => None,
        }
    }
}

impl Primitive for Address {
    fn to_word(&self) -> Word {
        let mut word = Word::default();
        word[12..].copy_from_slice(self.as_ref());
        word
    }

    fn from_word(word: Word) -> Option<Self> {
        (word[..12] == [0; 12]).then_some(Self::from_slice(&word[12..]))
    }
}

impl Primitive for Digest {
    fn to_word(&self) -> Word {
        **self
    }

    fn from_word(word: Word) -> Option<Self> {
        Some(Self(word))
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
