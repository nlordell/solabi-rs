//! Solidity ABI error encoding.

use crate::{
    decode::{Decode, DecodeError},
    encode::Encode,
    function::Selector,
};
use std::{
    fmt::{self, Debug, Formatter},
    marker::PhantomData,
};

/// A typed error encoder.
pub struct ErrorEncoder<D> {
    /// The error selector.
    pub selector: Selector,
    _marker: PhantomData<*const D>,
}

impl<D> ErrorEncoder<D>
where
    D: Encode + Decode,
{
    /// Creates a new error encoder from a selector.
    pub fn new(selector: Selector) -> Self {
        Self {
            selector,
            _marker: PhantomData,
        }
    }

    /// Encodes a Solidity error for the specified data.
    pub fn encode(&self, data: &D) -> Vec<u8> {
        crate::encode_with_selector(self.selector, data)
    }

    /// Decodes a Solidity error from the return bytes call into its data.
    pub fn decode(&self, data: &[u8]) -> Result<D, DecodeError> {
        crate::decode_with_selector(self.selector, data)
    }
}

impl<D> Debug for ErrorEncoder<D> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_struct("ErrorEncoder")
            .field("selector", &self.selector)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ethaddr::{address, Address};
    use ethnum::U256;
    use hex_literal::hex;
    use std::borrow::Cow;

    #[test]
    fn round_trips_error_encoding() {
        let insufficient_balance = ErrorEncoder::<(Address, U256)>::new(Selector(hex!("f6deaa04")));

        let account = address!("0x0101010101010101010101010101010101010101");
        let balance = U256::new(4_200_000_000_000_000_000);

        let data = hex!(
            "f6deaa04
             0000000000000000000000000101010101010101010101010101010101010101
             0000000000000000000000000000000000000000000000003a4965bf58a40000"
        );

        assert_eq!(insufficient_balance.encode(&(account, balance)), data);
        assert_eq!(
            insufficient_balance.decode(&data).unwrap(),
            (account, balance)
        );
    }

    #[test]
    fn revert_error() {
        let revert = ErrorEncoder::<(Cow<str>,)>::new(Selector(hex!("08c379a0")));

        let message = Cow::Borrowed("revert");
        let fields = (message,);

        let data = hex!(
            "08c379a0
             0000000000000000000000000000000000000000000000000000000000000020
             0000000000000000000000000000000000000000000000000000000000000006
             7265766572740000000000000000000000000000000000000000000000000000"
        );

        assert_eq!(revert.encode(&fields), data);
        assert_eq!(revert.decode(&data).unwrap(), fields);
    }
}
