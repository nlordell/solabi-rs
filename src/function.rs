//! Solidity ABI function pointer type.

use crate::{
    decode::{Decode, DecodeError},
    encode::Encode,
    fmt::Hex,
    primitive::{Primitive, Word},
};
use ethaddr::Address;
use std::{
    fmt::{self, Debug, Formatter},
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

/// A function selector type.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Selector(pub [u8; 4]);

impl AsRef<[u8]> for Selector {
    fn as_ref(&self) -> &[u8] {
        &self.0[..]
    }
}

impl From<[u8; 4]> for Selector {
    fn from(bytes: [u8; 4]) -> Self {
        Self(bytes)
    }
}

impl Deref for Selector {
    type Target = [u8; 4];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Selector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialEq<[u8]> for Selector {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_ref() == other
    }
}

impl PartialEq<[u8; 4]> for Selector {
    fn eq(&self, other: &[u8; 4]) -> bool {
        self.0 == *other
    }
}

impl Debug for Selector {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("Selector").field(&Hex(&self.0)).finish()
    }
}

/// Recent Solidity ABI version support function pointers. They are encoded as
/// a contract address and a selector packed into a `bytes24`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ExternalFunction {
    /// The contract address that will be used when calling this function
    /// pointer.
    pub address: Address,
    /// The selector of the contract function getting called.
    pub selector: Selector,
}

impl Primitive for ExternalFunction {
    fn to_word(&self) -> Word {
        let mut word = Word::default();
        word[..20].copy_from_slice(self.address.as_ref());
        word[20..24].copy_from_slice(self.selector.as_ref());
        word
    }

    fn cast(word: Word) -> Self {
        let mut address = Address::default();
        let mut selector = Selector(Default::default());
        address.copy_from_slice(&word[..20]);
        selector.0.copy_from_slice(&word[20..24]);
        Self { address, selector }
    }
}

/// A typed function encoder.
pub struct FunctionEncoder<P, R> {
    /// The function selector.
    pub selector: Selector,
    _marker: PhantomData<*const (P, R)>,
}

impl<P, R> FunctionEncoder<P, R>
where
    P: Encode + Decode,
    R: Encode + Decode,
{
    /// Creates a new function encoder from a selector.
    pub fn new(selector: Selector) -> Self {
        Self {
            selector,
            _marker: PhantomData,
        }
    }

    /// Encodes a function call for the specified parameters.
    pub fn encode_params(&self, params: &P) -> Vec<u8> {
        crate::encode_with_selector(self.selector, params)
    }

    /// Decodes a function call into its parameters.
    pub fn decode_params(&self, data: &[u8]) -> Result<P, DecodeError> {
        crate::decode_with_selector(self.selector, data)
    }

    /// Encodes function return data.
    pub fn encode_returns(&self, returns: &R) -> Vec<u8> {
        crate::encode(returns)
    }

    /// Decodes function return data.
    pub fn decode_returns(&self, data: &[u8]) -> Result<R, DecodeError> {
        crate::decode(data)
    }
}

impl<P, R> Debug for FunctionEncoder<P, R> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_struct("FunctionEncoder")
            .field("selector", &self.selector)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytes::Bytes;
    use ethaddr::{address, Address};
    use ethnum::U256;
    use hex_literal::hex;
    use std::borrow::Cow;

    #[test]
    fn transfer_encoder() {
        let transfer = FunctionEncoder::<(Address, U256), (bool,)>::new(Selector(hex!("a9059cbb")));

        let to = address!("0x0101010101010101010101010101010101010101");
        let value = U256::new(4_200_000_000_000_000_000);

        let call = hex!(
            "a9059cbb
             0000000000000000000000000101010101010101010101010101010101010101
             0000000000000000000000000000000000000000000000003a4965bf58a40000"
        );

        assert_eq!(transfer.encode_params(&(to, value)), call);
        assert_eq!(transfer.decode_params(&call).unwrap(), (to, value));

        let success = true;

        let ret = hex!("0000000000000000000000000000000000000000000000000000000000000001");

        assert_eq!(transfer.encode_returns(&(success,)), ret);
        assert_eq!(transfer.decode_returns(&ret).unwrap(), (success,));
    }

    #[test]
    fn unit_function_types() {
        let selector = [0; 4];
        let unit = FunctionEncoder::<(), ()>::new(Selector(selector));

        assert_eq!(unit.encode_params(&()), selector);
        assert!(unit.decode_params(&selector).is_ok());
        assert_eq!(unit.encode_returns(&()), b"");
        assert!(unit.decode_returns(&[]).is_ok());
    }

    #[test]
    fn errors_on_incorrect_selector() {
        let unit = FunctionEncoder::<(), ()>::new(Selector([1; 4]));
        assert!(unit.decode_params(&[2; 4]).is_err());
    }

    #[test]
    fn encode_borrowed_decode_owned() {
        let function = FunctionEncoder::<
            (Cow<str>, Cow<Bytes<[u8]>>, Cow<[U256]>, Cow<[String; 3]>),
            (),
        >::new(Selector([0; 4]));

        let owned = (
            Cow::<str>::Owned("moo".to_owned()),
            Cow::<Bytes<[u8]>>::Owned(Bytes(vec![1, 2, 3])),
            Cow::<[U256]>::Owned(vec![U256::MIN, U256::ONE, U256::MAX]),
            Cow::<[String; 3]>::Owned(["a".to_owned(), "b".to_owned(), "c".to_owned()]),
        );
        let borrowed = (
            Cow::Borrowed(&*owned.0),
            Cow::Borrowed(&*owned.1),
            Cow::Borrowed(&*owned.2),
            Cow::Borrowed(&*owned.3),
        );

        let data = hex!(
            "00000000
             0000000000000000000000000000000000000000000000000000000000000080
             00000000000000000000000000000000000000000000000000000000000000c0
             0000000000000000000000000000000000000000000000000000000000000100
             0000000000000000000000000000000000000000000000000000000000000180
             0000000000000000000000000000000000000000000000000000000000000003
             6d6f6f0000000000000000000000000000000000000000000000000000000000
             0000000000000000000000000000000000000000000000000000000000000003
             0102030000000000000000000000000000000000000000000000000000000000
             0000000000000000000000000000000000000000000000000000000000000003
             0000000000000000000000000000000000000000000000000000000000000000
             0000000000000000000000000000000000000000000000000000000000000001
             ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
             0000000000000000000000000000000000000000000000000000000000000060
             00000000000000000000000000000000000000000000000000000000000000a0
             00000000000000000000000000000000000000000000000000000000000000e0
             0000000000000000000000000000000000000000000000000000000000000001
             6100000000000000000000000000000000000000000000000000000000000000
             0000000000000000000000000000000000000000000000000000000000000001
             6200000000000000000000000000000000000000000000000000000000000000
             0000000000000000000000000000000000000000000000000000000000000001
             6300000000000000000000000000000000000000000000000000000000000000"
        );

        assert_eq!(function.encode_params(&owned), data);
        assert_eq!(function.encode_params(&borrowed), data);

        let decoded = function.decode_params(&data).unwrap();

        assert_eq!(decoded, owned);
        assert!(matches!(decoded.0, Cow::Owned(_)));
        assert!(matches!(decoded.1, Cow::Owned(_)));
        assert!(matches!(decoded.2, Cow::Owned(_)));
        assert!(matches!(decoded.3, Cow::Owned(_)));
    }
}
