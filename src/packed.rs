//! Module implementing packed encoding.

use crate::encode::BufferSizeError;

pub trait EncodePacked {
    fn encode_packed(&self, out: &mut [u8]);

    fn packed_size(&self) -> usize;
}

pub fn encode_packed<T: EncodePacked>(value: &T) -> Vec<u8> {
    let mut buf = vec![0u8; value.packed_size()];
    encode_packed_to(&mut buf, value).unwrap();
    buf
}

pub fn encode_packed_to<T>(buffer: &mut [u8], value: &T) -> Result<(), BufferSizeError>
where
    T: EncodePacked,
{
    if buffer.len() != value.packed_size() {
        return Err(BufferSizeError);
    }

    value.encode_packed(buffer);
    Ok(())
}

macro_rules! impl_encode_packed_for_integer {
    ($($t:ty),* $(,)?) => {
        $(
            impl EncodePacked for $t {
                fn encode_packed(&self, out: &mut [u8]) {
                    out.copy_from_slice(&self.to_be_bytes());
                }

                fn packed_size(&self) -> usize {
                    std::mem::size_of::<Self>()
                }
            }
        )*
    };
}

impl_encode_packed_for_integer!(
    u8, u16, u32, u64, u128, usize,
    i8, i16, i32, i64, i128, isize,
);

impl<T, const N: usize> EncodePacked for [T; N]
where
    T: EncodePacked,
{
    fn packed_size(&self) -> usize {
        self.iter().map(|item| item.packed_size()).sum()
    }

    fn encode_packed(&self, out: &mut [u8]) {
        let mut offset = 0;
        for item in self {
            let end = offset + item.packed_size();
            item.encode_packed(&mut out[offset..end]);
            offset = end;
        }
    }
}

macro_rules! impl_encode_packed_for_ref {
    () => {
        fn packed_size(&self) -> usize {
            (**self).packed_size()
        }

        fn encode_packed(&self, out: &mut [u8]) {
            (**self).encode_packed(out)
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use hex_literal::hex;

    #[test]
    fn encode_basic_types() {
        assert_eq!(encode_packed(&0u8), hex!("00"));
        assert_eq!(encode_packed(&255u8), hex!("ff"));
        assert_eq!(encode_packed(&0x1234u16), hex!("1234"));
        assert_eq!(encode_packed(&0x01020304i32), hex!("01020304"));
        assert_eq!(encode_packed(&-1i32), hex!("ffffffff"));
        let encoded = encode_packed(&1usize);
        // Generate hex literal of length based on usize
        let usize_result = if std::mem::size_of::<usize>() == 4 {
            hex!("00000001").to_vec()
        } else {
            hex!("0000000000000001").to_vec()
        };
        assert!(encoded == usize_result);
        assert_eq!(encode_packed(&-128i8), hex!("80"));
        assert_eq!(encode_packed(&127i8), hex!("7f"));

        // Tests for array

        assert_eq!(encode_packed(&[1u8, 2, 3]), hex!("010203"));
    }
}