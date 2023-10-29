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
//!     encode::{Encode, Encoder, Size},
//!     decode::{Decode, DecodeError, Decoder},
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
//! ```
//!
//! > There are plans to provide `Encode` and `Decode` procedural macros to
//! > automatically implement these traits in the future.

pub mod abi;
pub mod bytes;
pub mod constructor;
pub mod decode;
pub mod encode;
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
        ($value:expr, $encoded:expr $(,)?) => {{
            let (value, encoded) = ($value, $encoded);
            assert_eq!(encode(&value), encoded);

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
        );

        assert_encoding!(
            "hello".to_owned(),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000005
                 68656c6c6f000000000000000000000000000000000000000000000000000000"
            ),
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
        );

        assert_encoding!(
            98127491_i32,
            hex!("0000000000000000000000000000000000000000000000000000000005d94e83"),
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
        );

        assert_encoding!(
            0_u8,
            hex!("0000000000000000000000000000000000000000000000000000000000000000"),
        );
        assert_encoding!(
            1_u8,
            hex!("0000000000000000000000000000000000000000000000000000000000000001"),
        );
        assert_encoding!(
            2_u8,
            hex!("0000000000000000000000000000000000000000000000000000000000000002"),
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
        );

        assert_encoding!(
            ("abcdef".to_owned(),),
            hex!(
                "0000000000000000000000000000000000000000000000000000000000000020
                 0000000000000000000000000000000000000000000000000000000000000006
                 6162636465660000000000000000000000000000000000000000000000000000"
            ),
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
        );

        assert_encoding!(
            0_i32,
            hex!("0000000000000000000000000000000000000000000000000000000000000000"),
        );
        assert_encoding!(
            1_i32,
            hex!("0000000000000000000000000000000000000000000000000000000000000001"),
        );
        assert_encoding!(
            7_i32,
            hex!("0000000000000000000000000000000000000000000000000000000000000007"),
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
        );

        assert_encoding!((), hex!(""));
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

                assert_encoding!((vec![Bytes(*b"abc"), Bytes(*b"def")], y), buffer);
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

                    assert_encoding!((y, vec![Bytes(*b"abc"), Bytes(*b"def")]), buffer);
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
        );
    }
}
