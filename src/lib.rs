//! Solidity ABI encoding and decoding implementation.

#[cfg(test)]
#[macro_use]
mod testutil;

pub mod decode;
pub mod encode;
pub mod layout;
pub mod types;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
