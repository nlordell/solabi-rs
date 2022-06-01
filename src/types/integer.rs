//! Solidity primitive integer types.

use super::{Primitive, Word};
use ethaddr::Address;
use ethnum::{AsU256 as _, I256, U256};

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

impl Primitive for Address {
    fn to_word(&self) -> Word {
        let mut word = Word::default();
        word[12..].copy_from_slice(&**self);
        word
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
