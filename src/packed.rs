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
    if buffer.len() < value.packed_size() {
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
            item.encode_packed(&mut out[offset..offset + item.packed_size()]);
            offset += item.packed_size();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_basic_types() {
        assert_eq!(encode_packed(&0u8), vec![0]);
        assert_eq!(encode_packed(&255u8), vec![255]);
        assert_eq!(encode_packed(&0x1234u16), vec![0x12, 0x34]);
        assert_eq!(encode_packed(&0x01020304i32), vec![0x01, 0x02, 0x03, 0x04]);
        assert_eq!(encode_packed(&-1i32), vec![0xff, 0xff, 0xff, 0xff]);
        let encoded = encode_packed(&1usize);
        assert!(encoded == vec![0, 0, 0, 1] || encoded == vec![0, 0, 0, 0, 0, 0, 0, 1]);
        assert_eq!(encode_packed(&-128i8), vec![0x80]);
        assert_eq!(encode_packed(&127i8), vec![0x7f]);

        // Tests for array

        assert_eq!(encode_packed(&[1u8, 2, 3]), vec![1, 2, 3]);
    }
}