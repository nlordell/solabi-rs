//! Solidity ABI constructor encoding.

use crate::{
    decode::{Decode, DecodeError},
    encode::Encode,
    fmt::Hex,
};
use std::{
    fmt::{self, Debug, Formatter},
    marker::PhantomData,
};

/// A typed constructor encoder.
pub struct ConstructorEncoder<B, P> {
    /// The constructor deployment code.
    pub code: B,
    _marker: PhantomData<*const P>,
}

impl<B, P> ConstructorEncoder<B, P>
where
    B: AsRef<[u8]>,
    P: Encode + Decode,
{
    /// Creates a new constructor encoder from a selector.
    pub fn new(code: B) -> Self {
        Self {
            code,
            _marker: PhantomData,
        }
    }

    /// Encodes a contract deployment for the specified parameters.
    pub fn encode(&self, data: &P) -> Vec<u8> {
        crate::encode_with_prefix(self.code.as_ref(), data)
    }

    /// Encodes a contract deployment parameters without any code.
    pub fn encode_params(&self, data: &P) -> Vec<u8> {
        crate::encode(data)
    }

    /// Decodes the contract deployment parameters from the specified calldata.
    pub fn decode(&self, data: &[u8]) -> Result<P, DecodeError> {
        crate::decode_with_prefix(self.code.as_ref(), data)
    }

    /// Decodes the contract deployment parameters without any code.
    pub fn decode_params(&self, data: &[u8]) -> Result<P, DecodeError> {
        crate::decode(data)
    }
}

impl<B, P> Debug for ConstructorEncoder<B, P>
where
    B: AsRef<[u8]>,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_struct("ConstructorEncoder")
            .field("code", &Hex(self.code.as_ref()))
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ethaddr::{address, Address};
    use hex_literal::hex;

    #[test]
    fn round_trips_deployment_encoding() {
        let code = hex!(
            "60a060405234801561001057600080fd5b506040516101083803806101088339
             8101604081905261002f91610040565b6001600160a01b031660805261007056
             5b60006020828403121561005257600080fd5b81516001600160a01b03811681
             1461006957600080fd5b9392505050565b608051608061008860003960006006
             015260806000f3fe60806040527f000000000000000000000000000000000000
             00000000000000000000000000003660008037600080366000845af43d600080
             3e80600181146045573d6000fd5b3d6000f3fea264697066735822122007589b
             6aeb4b41bc48a82fc5939d02ccb42a23fd27c8bf5430706f182fd9a47164736f
             6c63430008100033"
        );
        let proxy = ConstructorEncoder::<_, (Address,)>::new(code);

        let account = address!("0x0101010101010101010101010101010101010101");

        let params = hex!("0000000000000000000000000101010101010101010101010101010101010101");
        let call = [&code[..], &params[..]].concat();

        assert_eq!(proxy.encode(&(account,)), call);
        assert_eq!(proxy.encode_params(&(account,)), params);
        assert_eq!(proxy.decode(&call).unwrap(), (account,));
        assert_eq!(proxy.decode_params(&params).unwrap(), (account,));
    }
}
