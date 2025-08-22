//! Module implementing packed encoding.

pub trait EncodePacked {
    fn encode_packed(&self, out: &mut Vec<u8>);
}

pub fn encode_packed<T: EncodePacked>(value: &T) -> Vec<u8> {
    let mut buf = Vec::new();
    value.encode_packed(&mut buf);
    buf
}

macro_rules! impl_encode_packed_for_integer {
    ($($t:ty),* $(,)?) => {
        $(
            impl EncodePacked for $t {
                fn encode_packed(&self, out: &mut Vec<u8>) {
                    out.extend_from_slice(&self.to_be_bytes());
                }
            }
        )*
    };
}

impl_encode_packed_for_integer!(
    u8, u16, u32, u64, u128, usize,
    i8, i16, i32, i64, i128, isize,
);

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
    }
}