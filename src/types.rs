//! Solidity ABI specific types.

pub mod address;
pub mod bytes;
pub mod function;
pub mod integer;

use ethnum::{AsU256, I256, U256};

/// An Ethereum 32-byte word.
pub type Word = [u8; 32];

/// Trait reprenting any type that can be converted to and from a single
/// Ethereum 32-byte word.
trait Primitive {
    fn to_word(&self) -> Word;
}

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
    i8, i16, i32, i64, i128, I256,
    u8, u16, u32, u64, u128, U256,
    bool,
}
