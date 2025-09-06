//! Solidity ABI encoding and decoding implementation.
//!
//! At a high level, this crate provides Solidity ABI [`encode()`]-ing and
//! [`decode()`]-ing implementations for a collection of primitive types, as
//! well as generic implementations for various collections and tuple types.
//!
//! ```
//! # use solabi::*;
//! let encoded = solabi::encode(&(42_i32, true, Bytes::borrowed(&[0, 1, 2, 3])));
//! let (a, b, c): (i32, bool, Bytes<Vec<u8>>) = solabi::decode(&encoded).unwrap();
//! assert_eq!(a, 42);
//! assert_eq!(b, true);
//! assert_eq!(c.as_bytes(), [0, 1, 2, 3]);
//! ```
//!
//! # Encoders
//!
//! Furthermore, this library provides encoders for various Solidity ABI items:
//! - Functions ([`FunctionEncoder`] and [`ConstructorEncoder`]).
//! - Events ([`EventEncoder`] and [`AnonymousEventEncoder`]).
//! - Errors ([`ErrorEncoder`]).
//!
//! These encoders provide a type-safe interface for Solidity encoding and
//! decoding their parameters.
//!
//! ```
//! # use solabi::*;
//! const TRANSFER: FunctionEncoder<(Address, U256), (bool,)> =
//!     FunctionEncoder::new(selector!("transfer(address,uint256)"));
//!
//! let call = TRANSFER.encode_params(&(
//!     address!("0x0101010101010101010101010101010101010101"),
//!     uint!("4_200_000_000_000_000_000"),
//! ));
//! # let _ = call;
//! ```
//!
//! # Dynamic Values
//!
//! The [`value::Value`] type provides dynamic Solidity values. This allows
//! Solidity ABI encoding and decoding when types are not known at compile-time.
//!
//! ```
//! # use solabi::*;
//! let event = abi::EventDescriptor::parse_declaration(
//!     "event Transfer(address indexed to, address indexed from, uint256 value)",
//! )
//! .unwrap();
//! let encoder = value::EventEncoder::new(&event).unwrap();
//! let log = encoder
//!     .encode(&[
//!         Value::Address(address!("0x0101010101010101010101010101010101010101")),
//!         Value::Address(address!("0x0202020202020202020202020202020202020202")),
//!         Value::Uint(value::Uint::new(256, uint!("4_200_000_000_000_000_000")).unwrap()),
//!     ])
//!     .unwrap();
//! # let _ = log;
//! ```
//!
//! # Custom Encoding Implementation
//!
//! It can be useful to define custom types that encode and decode to the
//! Solidity ABI. This can be done by implementing the [`encode::Encode`] and
//! [`decode::Decode`] traits.
//!
//! ```
//! use solabi::{
//!     decode::{Decode, DecodeError, Decoder},
//!     encode::{Encode, Encoder, Size},
//!     encode_packed::EncodePacked,
//! };
//!
//! #[derive(Debug, Eq, PartialEq)]
//! struct MyStruct {
//!     a: u64,
//!     b: String,
//! }
//!
//! impl Encode for MyStruct {
//!     fn size(&self) -> Size {
//!         (self.a, self.b.as_str()).size()
//!     }
//!
//!     fn encode(&self, encoder: &mut Encoder) {
//!         (self.a, self.b.as_str()).encode(encoder);
//!     }
//! }
//!
//! impl EncodePacked for MyStruct {
//!     fn packed_size(&self) -> usize {
//!         (self.a, self.b.as_str()).packed_size()
//!     }
//!
//!     fn encode_packed(&self, out: &mut [u8]) {
//!         (self.a, self.b.as_str()).encode_packed(out);
//!     }
//! }
//!
//! impl Decode for MyStruct {
//!     fn is_dynamic() -> bool {
//!         <(u64, String)>::is_dynamic()
//!     }
//!
//!     fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError> {
//!         let (a, b) = Decode::decode(decoder)?;
//!         Ok(Self { a, b })
//!     }
//! }
//!
//! let my_struct = MyStruct {
//!     a: 42,
//!     b: "The Answer to Life the Universe and Everything".to_string(),
//! };
//!
//! let encoded = solabi::encode(&my_struct);
//! let decoded = solabi::decode(&encoded).unwrap();
//!
//! assert_eq!(my_struct, decoded);
//!
//! let packed = solabi::encode_packed(&my_struct);
//!
//! assert_eq!(
//!     packed,
//!     b"\x00\x00\x00\x00\x00\x00\x00\x2a\
//!       The Answer to Life the Universe and Everything",
//! );
//! ```
//!
//! > There are plans to provide `Encode` and `Decode` procedural macros to
//! > automatically implement these traits in the future.

pub mod abi;
pub mod bitint;
pub mod bytes;
pub mod constructor;
pub mod decode;
pub mod encode;
pub mod encode_packed;
pub mod error;
pub mod event;
mod fmt;
#[macro_use]
pub mod function;
pub mod log;
pub mod primitive;
pub mod value;

/// The `solabi` prelude.
pub mod prelude {
    pub use crate::{
        bitint::{
            Int104, Int112, Int120, Int128, Int136, Int144, Int152, Int16, Int160, Int168, Int176,
            Int184, Int192, Int200, Int208, Int216, Int224, Int232, Int24, Int240, Int248, Int256,
            Int32, Int40, Int48, Int56, Int64, Int72, Int8, Int80, Int88, Int96, Uint104, Uint112,
            Uint120, Uint128, Uint136, Uint144, Uint152, Uint16, Uint160, Uint168, Uint176,
            Uint184, Uint192, Uint200, Uint208, Uint216, Uint224, Uint232, Uint24, Uint240,
            Uint248, Uint256, Uint32, Uint40, Uint48, Uint56, Uint64, Uint72, Uint8, Uint80,
            Uint88, Uint96,
        },
        bytes::Bytes,
        constructor::ConstructorEncoder,
        error::ErrorEncoder,
        event::{AnonymousEventEncoder, EventEncoder},
        function::{ExternalFunction, FunctionEncoder, Selector},
        log::{Log, Topics},
        value::{Value, ValueKind},
    };
    pub use ethprim::prelude::*;
}

pub use self::{
    decode::{decode, decode_with_prefix, decode_with_selector},
    encode::{encode, encode_to, encode_with_prefix, encode_with_selector},
    encode_packed::{encode_packed, encode_packed_to},
    prelude::*,
};
pub use ethprim::{self, address, digest, int, keccak, uint};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        bytes::Bytes,
        function::{ExternalFunction, Selector},
        primitive::Primitive as _,
    };
    use ethprim::{address, uint};
    use hex_literal::hex;

    macro_rules! assert_encoding {
        ($value:expr, $encoded:expr, $packed:expr $(,)?) => {{
            let (value, encoded, packed) = ($value, $encoded, $packed);
            assert_eq!(encode(&value), encoded);
            assert_eq!(encode_packed(&value), packed);

            // Small work around to avoid manually specifying types.
            #[allow(unused_assignments)]
            let decoded = {
                let mut result = value.clone();
                result = decode(&encoded).unwrap();
                result
            };
            assert_eq!(decoded, value);
        }};
    }

    #[test]
    fn the_answer_to_life_the_universe_everything() {
        assert_encoding!(
            uint!("42"),
            hex!("000000000000000000000000000000000000000000000000000000000000002a"),
            hex!("000000000000000000000000000000000000000000000000000000000000002a"),
        );
    }

    #[test]
    fn root_has_no_jump() {
        assert_encoding!(
            ("hello".to_owned(),),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000020
                 0000000000000000000000000000000000000000000000000000000000000005
                 68656c6c6f000000000000000000000000000000000000000000000000000000"
            ),
            hex!("68656c6c6f"),
        );

        assert_encoding!(
            "hello".to_owned(),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000005
                 68656c6c6f000000000000000000000000000000000000000000000000000000"
            ),
            hex!("68656c6c6f"),
        );
    }

    #[test]
    fn ethereum_basic_abi_tests() {
        // <https://github.com/ethereum/tests/blob/0e8d25bb613cab7f9e99430f970e1e6cbffdbf1a/ABITests/basic_abi_tests.json>

        assert_encoding!(
            (
                291_i32,
                vec![1110_i32, 1929_i32],
                Bytes(*b"1234567890"),
                "Hello, world!".to_owned(),
            ),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000123
                 0000000000000000000000000000000000000000000000000000000000000080
                 3132333435363738393000000000000000000000000000000000000000000000
                 00000000000000000000000000000000000000000000000000000000000000e0
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000456
                 0000000000000000000000000000000000000000000000000000000000000789
                 000000000000000000000000000000000000000000000000000000000000000d
                 48656c6c6f2c20776f726c642100000000000000000000000000000000000000"
            ),
            hex!(
                "0000012300000456000007893132333435363738393048656c6c6f2c20776f72
                 6c6421"
            ),
        );

        assert_encoding!(
            98127491_i32,
            hex!("0000000000000000000000000000000000000000000000000000000005d94e83"),
            hex!("05d94e83"),
        );

        assert_encoding!(
            (
                324124_i32,
                address!("0xCD2a3d9F938E13CD947Ec05AbC7FE734Df8DD826")
            ),
            hex!(
                "000000000000000000000000000000000000000000000000000000000004f21c
                 000000000000000000000000cd2a3d9f938e13cd947ec05abc7fe734df8dd826"
            ),
            hex!("0004f21ccd2a3d9f938e13cd947ec05abc7fe734df8dd826"),
        );
    }

    #[test]
    fn solidity_abi_encoder_tests() {
        // <https://github.com/ethereum/solidity/blob/43f29c00da02e19ff10d43f7eb6955d627c57728/test/libsolidity/ABIEncoderTests.cpp>

        assert_encoding!(
            (
                10_i32,
                u16::MAX - 1,
                0x121212_i32,
                -1_i32,
                Bytes(hex!("1babab")),
                true,
                address!(~"0xfffffffffffffffffffffffffffffffffffffffb")
            ),
            hex!(
                "000000000000000000000000000000000000000000000000000000000000000a
                 000000000000000000000000000000000000000000000000000000000000fffe
                 0000000000000000000000000000000000000000000000000000000000121212
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 1babab0000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000001
                 000000000000000000000000fffffffffffffffffffffffffffffffffffffffb"
            ),
            hex!(
                "0000000afffe00121212ffffffff1babab01ffffffffffffffffffffffffffff
                 fffffffffffb"
            ),
        );

        assert_encoding!(
            (
                "abcdef".to_owned(),
                Bytes(*b"abcde\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"),
                "abcdefabcdefgehabcabcasdfjklabcdefabcedefghabcabcasdfjklabcdefab\
                 cdefghabcabcasdfjklabcdeefabcdefghabcabcasdefjklabcdefabcdefghab\
                 cabcasdfjkl"
                    .to_owned(),
            ),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000060
                 6162636465000000000000000000000000000000000000000000000000000000
                 00000000000000000000000000000000000000000000000000000000000000a0
                 0000000000000000000000000000000000000000000000000000000000000006
                 6162636465660000000000000000000000000000000000000000000000000000
                 000000000000000000000000000000000000000000000000000000000000008b
                 616263646566616263646566676568616263616263617364666a6b6c61626364
                 6566616263656465666768616263616263617364666a6b6c6162636465666162
                 636465666768616263616263617364666a6b6c61626364656566616263646566
                 676861626361626361736465666a6b6c61626364656661626364656667686162
                 63616263617364666a6b6c000000000000000000000000000000000000000000"
            ),
            hex!(
                "6162636465666162636465000000000000000000000000000000616263646566
                 616263646566676568616263616263617364666a6b6c61626364656661626365
                 6465666768616263616263617364666a6b6c6162636465666162636465666768
                 616263616263617364666a6b6c61626364656566616263646566676861626361
                 626361736465666a6b6c61626364656661626364656667686162636162636173
                 64666a6b6c"
            ),
        );

        assert_encoding!(
            0_u8,
            hex!("0000000000000000000000000000000000000000000000000000000000000000"),
            hex!("00"),
        );
        assert_encoding!(
            1_u8,
            hex!("0000000000000000000000000000000000000000000000000000000000000001"),
            hex!("01"),
        );
        assert_encoding!(
            2_u8,
            hex!("0000000000000000000000000000000000000000000000000000000000000002"),
            hex!("02"),
        );

        assert_encoding!(
            (
                Bytes([0_u8, 0, 0, 10]),
                Bytes(hex!("f1f20000")),
                0xff_u8,
                0xff_u8,
                -1_i8,
                1_i8,
            ),
            hex!(
                "0000000a00000000000000000000000000000000000000000000000000000000
                 f1f2000000000000000000000000000000000000000000000000000000000000
                 00000000000000000000000000000000000000000000000000000000000000ff
                 00000000000000000000000000000000000000000000000000000000000000ff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 0000000000000000000000000000000000000000000000000000000000000001"
            ),
            hex!("0000000af1f20000ffffff01"),
        );

        assert_encoding!(
            (
                10_i32,
                vec![0xfffffffe_u64, 0xffffffff, 0x100000000],
                11_i32
            ),
            hex!(
                "000000000000000000000000000000000000000000000000000000000000000a
                 0000000000000000000000000000000000000000000000000000000000000060
                 000000000000000000000000000000000000000000000000000000000000000b
                 0000000000000000000000000000000000000000000000000000000000000003
                 00000000000000000000000000000000000000000000000000000000fffffffe
                 00000000000000000000000000000000000000000000000000000000ffffffff
                 0000000000000000000000000000000000000000000000000000000100000000"
            ),
            hex!("0000000a00000000fffffffe00000000ffffffff00000001000000000000000b"),
        );

        assert_encoding!(
            (10_i32, [vec![7_i16, 0x0506, -1], vec![4, 5]], 11_i32),
            hex!(
                "000000000000000000000000000000000000000000000000000000000000000a
                 0000000000000000000000000000000000000000000000000000000000000060
                 000000000000000000000000000000000000000000000000000000000000000b
                 0000000000000000000000000000000000000000000000000000000000000040
                 00000000000000000000000000000000000000000000000000000000000000c0
                 0000000000000000000000000000000000000000000000000000000000000003
                 0000000000000000000000000000000000000000000000000000000000000007
                 0000000000000000000000000000000000000000000000000000000000000506
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000004
                 0000000000000000000000000000000000000000000000000000000000000005"
            ),
            hex!("0000000a00070506ffff000400050000000b"),
        );

        assert_encoding!(
            (
                10_i32,
                vec![
                    "abcabcdefghjklmnopqrsuvwabcdefgijklmnopqrstuwabcdefgijklmnoprstuvw".to_owned(),
                    "abcdefghijklmnopqrtuvwabcfghijklmnopqstuvwabcdeghijklmopqrstuvw".to_owned(),
                ],
                11_i32,
            ),
            hex!(
                "000000000000000000000000000000000000000000000000000000000000000a
                 0000000000000000000000000000000000000000000000000000000000000060
                 000000000000000000000000000000000000000000000000000000000000000b
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000040
                 00000000000000000000000000000000000000000000000000000000000000c0
                 0000000000000000000000000000000000000000000000000000000000000042
                 61626361626364656667686a6b6c6d6e6f707172737576776162636465666769
                 6a6b6c6d6e6f7071727374757761626364656667696a6b6c6d6e6f7072737475
                 7677000000000000000000000000000000000000000000000000000000000000
                 000000000000000000000000000000000000000000000000000000000000003f
                 6162636465666768696a6b6c6d6e6f70717274757677616263666768696a6b6c
                 6d6e6f7071737475767761626364656768696a6b6c6d6f707172737475767700"
            ),
            hex!(
                "0000000a61626361626364656667686a6b6c6d6e6f7071727375767761626364
                 656667696a6b6c6d6e6f7071727374757761626364656667696a6b6c6d6e6f70
                 7273747576776162636465666768696a6b6c6d6e6f7071727475767761626366
                 6768696a6b6c6d6e6f7071737475767761626364656768696a6b6c6d6f707172
                 73747576770000000b"
            ),
        );

        assert_encoding!(
            (
                "123456789012345678901234567890a".to_owned(),
                "ffff123456789012345678901234567890afffffffff123456789012345678901234567890a"
                    .to_owned(),
            ),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000080
                 000000000000000000000000000000000000000000000000000000000000001f
                 3132333435363738393031323334353637383930313233343536373839306100
                 000000000000000000000000000000000000000000000000000000000000004b
                 6666666631323334353637383930313233343536373839303132333435363738
                 3930616666666666666666663132333435363738393031323334353637383930
                 3132333435363738393061000000000000000000000000000000000000000000"
            ),
            hex!(
                "3132333435363738393031323334353637383930313233343536373839306166
                 6666663132333435363738393031323334353637383930313233343536373839
                 3061666666666666666666313233343536373839303132333435363738393031
                 32333435363738393061"
            ),
        );

        assert_encoding!(
            [
                address!(~"0xffffffffffffffffffffffffffffffffffffffff"),
                address!(~"0xfffffffffffffffffffffffffffffffffffffffe"),
                address!(~"0xfffffffffffffffffffffffffffffffffffffffd"),
            ],
            hex!(
                "000000000000000000000000ffffffffffffffffffffffffffffffffffffffff
                 000000000000000000000000fffffffffffffffffffffffffffffffffffffffe
                 000000000000000000000000fffffffffffffffffffffffffffffffffffffffd"
            ),
            hex!(
                "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 fffffffffffffffefffffffffffffffffffffffffffffffffffffffd"
            ),
        );

        assert_encoding!(
            (vec![
                address!("0x0000000000000000000000000000000000000001"),
                address!("0x0000000000000000000000000000000000000002"),
                address!("0x0000000000000000000000000000000000000003"),
            ],),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000020
                 0000000000000000000000000000000000000000000000000000000000000003
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000003"
            ),
            hex!(
                "0000000000000000000000000000000000000001000000000000000000000000
                 00000000000000020000000000000000000000000000000000000003"
            ),
        );

        assert_encoding!(
            (vec![-1_i32, 2, -3, 4, -5, 6, -7, 8],),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000020
                 0000000000000000000000000000000000000000000000000000000000000008
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 0000000000000000000000000000000000000000000000000000000000000002
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd
                 0000000000000000000000000000000000000000000000000000000000000004
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffb
                 0000000000000000000000000000000000000000000000000000000000000006
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff9
                 0000000000000000000000000000000000000000000000000000000000000008"
            ),
            hex!("ffffffff00000002fffffffd00000004fffffffb00000006fffffff900000008"),
        );

        assert_encoding!(
            (
                ExternalFunction {
                    address: address!("0x001C08AB857afe5A9633887e7A4e2A429D1d8D42"),
                    selector: Selector(hex!("b3de648b")),
                },
                ExternalFunction {
                    address: address!("0x001C08AB857afe5A9633887e7A4e2A429D1d8D42"),
                    selector: Selector(hex!("b3de648b")),
                },
            ),
            hex!(
                "001c08ab857afe5a9633887e7a4e2a429d1d8d42b3de648b0000000000000000
                 001c08ab857afe5a9633887e7a4e2a429d1d8d42b3de648b0000000000000000"
            ),
            hex!(
                "001c08ab857afe5a9633887e7a4e2a429d1d8d42b3de648b001c08ab857afe5a
                 9633887e7a4e2a429d1d8d42b3de648b"
            ),
        );

        assert_encoding!(
            (
                ExternalFunction {
                    address: address!(~"0xffffffffffffffffffffffffffffffffffffffff"),
                    selector: Selector(hex!("ffffffff")),
                },
                ExternalFunction {
                    address: address!(~"0xffffffffffffffffffffffffffffffffffffffff"),
                    selector: Selector(hex!("ffffffff")),
                },
            ),
            hex!(
                "ffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000
                 ffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000"
            ),
            hex!(
                "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
                 ffffffffffffffffffffffffffffffff"
            ),
        );

        assert_encoding!(
            ("abcdef".to_owned(),),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000020
                 0000000000000000000000000000000000000000000000000000000000000006
                 6162636465660000000000000000000000000000000000000000000000000000"
            ),
            hex!("616263646566"),
        );
        assert_encoding!(
            (
                "abcdefgggggggggggggggggggggggggggggggggggggggghhheeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
                    .to_owned(),
            ),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000020
                 000000000000000000000000000000000000000000000000000000000000004f
                 6162636465666767676767676767676767676767676767676767676767676767
                 6767676767676767676767676767686868656565656565656565656565656565
                 6565656565656565656565656565650000000000000000000000000000000000"
            ),
            hex!(
                "6162636465666767676767676767676767676767676767676767676767676767
                 6767676767676767676767676767686868656565656565656565656565656565
                 656565656565656565656565656565"
            ),
        );

        assert_encoding!(
            0_i32,
            hex!("0000000000000000000000000000000000000000000000000000000000000000"),
            hex!("00000000"),
        );
        assert_encoding!(
            1_i32,
            hex!("0000000000000000000000000000000000000000000000000000000000000001"),
            hex!("00000001"),
        );
        assert_encoding!(
            7_i32,
            hex!("0000000000000000000000000000000000000000000000000000000000000007"),
            hex!("00000007"),
        );

        assert_encoding!(
            (
                7_i32,
                (
                    8_i32,
                    9_i32,
                    vec![(11_i32, 0_i32), (12, 0), (0, 13)],
                    10_i32,
                ),
            ),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000007
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000008
                 0000000000000000000000000000000000000000000000000000000000000009
                 0000000000000000000000000000000000000000000000000000000000000080
                 000000000000000000000000000000000000000000000000000000000000000a
                 0000000000000000000000000000000000000000000000000000000000000003
                 000000000000000000000000000000000000000000000000000000000000000b
                 0000000000000000000000000000000000000000000000000000000000000000
                 000000000000000000000000000000000000000000000000000000000000000c
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000
                 000000000000000000000000000000000000000000000000000000000000000d"
            ),
            hex!(
                "0000000700000008000000090000000b000000000000000c0000000000000000
                 0000000d0000000a"
            ),
        );

        assert_encoding!(
            (
                7_i32,
                [
                    (
                        address!("0x1111111111111111111111111111111111111111"),
                        vec![(0x11_i32, 1_i32, 0x12_i32)],
                    ),
                    Default::default(),
                ],
                vec![
                    Default::default(),
                    (
                        address!("0x0000000000000000000000000000000000001234"),
                        vec![(0_i32, 0_i32, 0_i32), (0x21, 2, 0x22), (0, 0, 0)]
                    ),
                ],
                8_i32,
            ),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000007
                 0000000000000000000000000000000000000000000000000000000000000080
                 00000000000000000000000000000000000000000000000000000000000001e0
                 0000000000000000000000000000000000000000000000000000000000000008
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000100
                 0000000000000000000000001111111111111111111111111111111111111111
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000011
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000012
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000040
                 00000000000000000000000000000000000000000000000000000000000000a0
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000001234
                 0000000000000000000000000000000000000000000000000000000000000040
                 0000000000000000000000000000000000000000000000000000000000000003
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000021
                 0000000000000000000000000000000000000000000000000000000000000002
                 0000000000000000000000000000000000000000000000000000000000000022
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000"
            ),
            hex!(
                "0000000711111111111111111111111111111111111111110000001100000001
                 0000001200000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000001234
                 0000000000000000000000000000002100000002000000220000000000000000
                 0000000000000008"
            )
        );

        assert_encoding!((), hex!(""), hex!(""));
        assert_encoding!(
            (vec![true, false, true, false], [true, false, true, false]),
            hex!(
                "00000000000000000000000000000000000000000000000000000000000000a0
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000004
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000000"
            ),
            hex!("0100010001000100")
        );

        macro_rules! bytes_nn_arrays {
            ([$($size:literal),*] { $($nn:literal),* }) => {
                bytes_nn_arrays!(__outer: [ $($size),* ] { $($nn,)* });
            };
            (__impl: $size:literal $nn:literal) => {{
                const SIZE: usize = $size;
                const NN: usize = $nn;

                let mut y: [Bytes<[u8; NN]>; SIZE] = Default::default();
                for (i, y) in y.iter_mut().enumerate() {
                    y.0[NN - 1] = (i as u8) + 1;
                }

                let mut buffer = Vec::new();
                buffer.extend_from_slice(&(0x20 * (1 + SIZE)).to_word());
                for y in &y {
                    buffer.extend_from_slice(&y.to_word());
                }
                buffer.extend_from_slice(&2.to_word());
                buffer.extend_from_slice(&Bytes(*b"abc").to_word());
                buffer.extend_from_slice(&Bytes(*b"def").to_word());

                let mut packed = Vec::new();
                packed.extend_from_slice(b"abc");
                packed.extend_from_slice(b"def");
                for y in &y {
                    packed.extend_from_slice(y.as_ref());
                }

                assert_encoding!(
                    (vec![Bytes(*b"abc"), Bytes(*b"def")], y),
                    buffer,
                    packed,
                );
            }};

            // Cartesian product of `$t * $n`
            (__outer: [ $($size:literal),* ] $rest:tt) => {$(
                bytes_nn_arrays!(__inner: $size $rest);
            )*};
            (__inner: $size:literal { $($nn:literal,)* }) => {$(
                bytes_nn_arrays!(__impl: $size $nn);
            )*};
        }

        bytes_nn_arrays!(
            [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
            {1, 2, 4, 5, 7, 15, 16, 17, 31, 32}
        );

        macro_rules! bytes_nn_arrays_dyn {
            ([ $size:expr ] { $($nn:literal),* }) => {$({
                let size = $size;
                const NN: usize = $nn;

                for size in size {
                    let y = (0..size)
                        .map(|i| {
                            let mut e = [0_u8; NN];
                            e[NN - 1] = (i as u8) + 1;
                            Bytes(e)
                        })
                        .collect::<Vec<_>>();

                    let mut buffer = Vec::new();
                    buffer.extend_from_slice(&(0x20 * 2).to_word());
                    buffer.extend_from_slice(&(0x20 * (3 + size)).to_word());
                    buffer.extend_from_slice(&size.to_word());
                    for y in &y {
                        buffer.extend_from_slice(&y.to_word());
                    }
                    buffer.extend_from_slice(&2.to_word());
                    buffer.extend_from_slice(&Bytes(*b"abc").to_word());
                    buffer.extend_from_slice(&Bytes(*b"def").to_word());

                    let mut packed = Vec::new();
                    for y in &y {
                        packed.extend_from_slice(y.as_ref());
                    }
                    packed.extend_from_slice(b"abc");
                    packed.extend_from_slice(b"def");

                    assert_encoding!(
                        (y, vec![Bytes(*b"abc"), Bytes(*b"def")]),
                        buffer,
                        packed,
                    );
                }
            })*};
        }

        bytes_nn_arrays_dyn!([1..15] {1, 2, 4, 5, 7, 15, 16, 17, 31, 32});

        assert_encoding!(
            (
                false,
                -5_i32,
                ExternalFunction {
                    address: address!("0x903D3a9A4266EB4432407ea5B1B4f80094f17957"),
                    selector: Selector(hex!("e2179b8e")),
                },
                Bytes(hex!("010203")),
                -3_i32,
            ),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000000
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffb
                 903d3a9a4266eb4432407ea5b1b4f80094f17957e2179b8e0000000000000000
                 0102030000000000000000000000000000000000000000000000000000000000
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd"
            ),
            hex!(
                "00fffffffb903d3a9a4266eb4432407ea5b1b4f80094f17957e2179b8e010203
                 fffffffd"
            ),
        );

        assert_encoding!(
            ("".to_owned(), 3_i32, "".to_owned()),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000060
                 0000000000000000000000000000000000000000000000000000000000000003
                 0000000000000000000000000000000000000000000000000000000000000080
                 0000000000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000000"
            ),
            hex!("00000003")
        );

        assert_encoding!(
            ("abc".to_owned(), 7_i32, "def".to_owned()),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000060
                 0000000000000000000000000000000000000000000000000000000000000007
                 00000000000000000000000000000000000000000000000000000000000000a0
                 0000000000000000000000000000000000000000000000000000000000000003
                 6162630000000000000000000000000000000000000000000000000000000000
                 0000000000000000000000000000000000000000000000000000000000000003
                 6465660000000000000000000000000000000000000000000000000000000000"
            ),
            hex!("61626300000007646566")
        );
    }

    #[test]
    fn bitints() {
        assert_encoding!(
            (
                Int24::new(1).unwrap(),
                Int40::new(-2).unwrap(),
                Int48::new(3).unwrap(),
                Int56::new(-4).unwrap(),
                Int72::new(5).unwrap(),
                Int80::new(-6).unwrap(),
                Int88::new(7).unwrap(),
                Int96::new(-8).unwrap(),
                Int104::new(9).unwrap(),
                Int112::new(-10).unwrap(),
                Int120::new(11).unwrap(),
            ),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000001
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe
                 0000000000000000000000000000000000000000000000000000000000000003
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc
                 0000000000000000000000000000000000000000000000000000000000000005
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa
                 0000000000000000000000000000000000000000000000000000000000000007
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8
                 0000000000000000000000000000000000000000000000000000000000000009
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff6
                 000000000000000000000000000000000000000000000000000000000000000b"
            ),
            hex!(
                "000001fffffffffe000000000003fffffffffffffc000000000000000005ffff
                 fffffffffffffffa0000000000000000000007fffffffffffffffffffffff800
                 000000000000000000000009fffffffffffffffffffffffffff6000000000000
                 00000000000000000b"
            ),
        );

        assert_encoding!(
            (
                Int136::new(int!("-12")).unwrap(),
                Int144::new(int!("13")).unwrap(),
                Int152::new(int!("-14")).unwrap(),
                Int160::new(int!("15")).unwrap(),
                Int168::new(int!("-16")).unwrap(),
                Int176::new(int!("17")).unwrap(),
                Int184::new(int!("-18")).unwrap(),
                Int192::new(int!("19")).unwrap(),
                Int200::new(int!("-20")).unwrap(),
                Int208::new(int!("21")).unwrap(),
                Int216::new(int!("-22")).unwrap(),
                Int224::new(int!("23")).unwrap(),
            ),
            hex!(
                "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff4
                 000000000000000000000000000000000000000000000000000000000000000d
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff2
                 000000000000000000000000000000000000000000000000000000000000000f
                 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0
                 0000000000000000000000000000000000000000000000000000000000000011
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffee
                 0000000000000000000000000000000000000000000000000000000000000013
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffec
                 0000000000000000000000000000000000000000000000000000000000000015
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffea
                 0000000000000000000000000000000000000000000000000000000000000017"
            ),
            hex!(
                "fffffffffffffffffffffffffffffffff4000000000000000000000000000000
                 00000dfffffffffffffffffffffffffffffffffffff200000000000000000000
                 0000000000000000000ffffffffffffffffffffffffffffffffffffffffff000
                 000000000000000000000000000000000000000011ffffffffffffffffffffff
                 ffffffffffffffffffffffee0000000000000000000000000000000000000000
                 00000013ffffffffffffffffffffffffffffffffffffffffffffffffec000000
                 0000000000000000000000000000000000000000000015ffffffffffffffffff
                 ffffffffffffffffffffffffffffffffffea0000000000000000000000000000
                 0000000000000000000000000017"
            ),
        );

        assert_encoding!(
            (
                Int232::new(int!("-24")).unwrap(),
                Int240::new(int!("25")).unwrap(),
                Int248::new(int!("-26")).unwrap(),
            ),
            hex!(
                "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe8
                 0000000000000000000000000000000000000000000000000000000000000019
                 ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe6"
            ),
            hex!(
                "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffe8000000
                 000000000000000000000000000000000000000000000000000019ffffffffff
                 ffffffffffffffffffffffffffffffffffffffffffffffffffe6"
            ),
        );

        assert_encoding!(
            (
                Uint24::new(27).unwrap(),
                Uint40::new(28).unwrap(),
                Uint48::new(29).unwrap(),
                Uint56::new(30).unwrap(),
                Uint72::new(31).unwrap(),
                Uint80::new(32).unwrap(),
                Uint88::new(33).unwrap(),
                Uint96::new(34).unwrap(),
                Uint104::new(35).unwrap(),
                Uint112::new(36).unwrap(),
                Uint120::new(37).unwrap(),
            ),
            hex!(
                "000000000000000000000000000000000000000000000000000000000000001b
                 000000000000000000000000000000000000000000000000000000000000001c
                 000000000000000000000000000000000000000000000000000000000000001d
                 000000000000000000000000000000000000000000000000000000000000001e
                 000000000000000000000000000000000000000000000000000000000000001f
                 0000000000000000000000000000000000000000000000000000000000000020
                 0000000000000000000000000000000000000000000000000000000000000021
                 0000000000000000000000000000000000000000000000000000000000000022
                 0000000000000000000000000000000000000000000000000000000000000023
                 0000000000000000000000000000000000000000000000000000000000000024
                 0000000000000000000000000000000000000000000000000000000000000025"
            ),
            hex!(
                "00001b000000001c00000000001d0000000000001e00000000000000001f0000
                 0000000000000020000000000000000000002100000000000000000000002200
                 0000000000000000000000230000000000000000000000000024000000000000
                 000000000000000025"
            ),
        );

        assert_encoding!(
            (
                Uint136::new(uint!("38")).unwrap(),
                Uint144::new(uint!("39")).unwrap(),
                Uint152::new(uint!("40")).unwrap(),
                Uint160::new(uint!("41")).unwrap(),
                Uint168::new(uint!("42")).unwrap(),
                Uint176::new(uint!("43")).unwrap(),
                Uint184::new(uint!("44")).unwrap(),
                Uint192::new(uint!("45")).unwrap(),
                Uint200::new(uint!("46")).unwrap(),
                Uint208::new(uint!("47")).unwrap(),
                Uint216::new(uint!("48")).unwrap(),
                Uint224::new(uint!("49")).unwrap(),
            ),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000026
                 0000000000000000000000000000000000000000000000000000000000000027
                 0000000000000000000000000000000000000000000000000000000000000028
                 0000000000000000000000000000000000000000000000000000000000000029
                 000000000000000000000000000000000000000000000000000000000000002a
                 000000000000000000000000000000000000000000000000000000000000002b
                 000000000000000000000000000000000000000000000000000000000000002c
                 000000000000000000000000000000000000000000000000000000000000002d
                 000000000000000000000000000000000000000000000000000000000000002e
                 000000000000000000000000000000000000000000000000000000000000002f
                 0000000000000000000000000000000000000000000000000000000000000030
                 0000000000000000000000000000000000000000000000000000000000000031"
            ),
            hex!(
                "0000000000000000000000000000000026000000000000000000000000000000
                 0000270000000000000000000000000000000000002800000000000000000000
                 0000000000000000002900000000000000000000000000000000000000002a00
                 00000000000000000000000000000000000000002b0000000000000000000000
                 00000000000000000000002c0000000000000000000000000000000000000000
                 0000002d0000000000000000000000000000000000000000000000002e000000
                 000000000000000000000000000000000000000000002f000000000000000000
                 0000000000000000000000000000000000300000000000000000000000000000
                 0000000000000000000000000031"
            ),
        );

        assert_encoding!(
            (
                Uint232::new(uint!("50")).unwrap(),
                Uint240::new(uint!("51")).unwrap(),
                Uint248::new(uint!("52")).unwrap(),
            ),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000032
                 0000000000000000000000000000000000000000000000000000000000000033
                 0000000000000000000000000000000000000000000000000000000000000034"
            ),
            hex!(
                "0000000000000000000000000000000000000000000000000000000032000000
                 0000000000000000000000000000000000000000000000000000330000000000
                 0000000000000000000000000000000000000000000000000034"
            ),
        );
    }
}
