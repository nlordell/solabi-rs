//! Module containing `Address` type and implementation.

use super::{Primitive, Word};
use hex::FromHexError;
use sha3::{Digest as _, Keccak256};
use std::{
    fmt::{self, Debug, Display, Formatter},
    ops::{Deref, DerefMut},
    str::{self, FromStr},
};

/// An Ethereum public address.
#[derive(Copy, Clone, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct Address(pub [u8; 20]);

impl Address {
    /// Creates an address from a slice.
    ///
    /// # Panics
    ///
    /// This method panics if the length of the slice is not 20 bytes.
    pub fn from_slice(slice: &[u8]) -> Self {
        let mut address = Self::default();
        address.as_mut().copy_from_slice(slice);
        address
    }
}

impl Debug for Address {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        struct DisplayAsDebug<T>(T);

        impl<T> Debug for DisplayAsDebug<T>
        where
            T: Display,
        {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        f.debug_tuple("Address")
            .field(&DisplayAsDebug(self))
            .finish()
    }
}

impl Display for Address {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut buf = *b"0x0000000000000000000000000000000000000000";
        let addr = &mut buf[2..];
        hex::encode_to_slice(self, addr).expect("error decoding hex");

        let digest = {
            let mut hasher = Keccak256::new();
            hasher.update(&addr);
            hasher.finalize()
        };
        for i in 0..addr.len() {
            let byte = digest[i / 2];
            let nibble = 0xf & if i % 2 == 0 { byte >> 4 } else { byte };
            if nibble >= 8 {
                addr[i] = addr[i].to_ascii_uppercase();
            }
        }

        f.write_str(unsafe { str::from_utf8_unchecked(&buf) })
    }
}

impl AsRef<[u8]> for Address {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl AsMut<[u8]> for Address {
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }
}

impl Deref for Address {
    type Target = [u8; 20];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Address {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FromStr for Address {
    type Err = FromHexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut address = Self::default();
        let s = s.strip_prefix("0x").unwrap_or(s);
        hex::decode_to_slice(s, address.as_mut())?;
        Ok(address)
    }
}

impl Primitive for Address {
    fn to_word(&self) -> Word {
        let mut word = Word::default();
        word[12..].copy_from_slice(self.as_ref());
        word
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn checksum_address() {
        for s in &[
            "0x90F8bf6A479f320ead074411a4B0e7944Ea8c9C1",
            "0xEeeeeEeeeEeEeeEeEeEeeEEEeeeeEeeeeeeeEEeE",
        ] {
            let address = s.parse::<Address>().unwrap();
            assert_eq!(address.to_string(), *s);
        }
    }
}
