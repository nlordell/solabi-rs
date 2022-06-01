//! Solidity ABI encoding and decoding implementation.

pub mod decode;
pub mod encode;
pub mod types;
pub mod value;

pub use self::{decode::decode, encode::encode};

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
