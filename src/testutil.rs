macro_rules! addr {
    ($($arg:tt)*) => {
        $crate::types::address::Address(::hex_literal::hex!($($arg)*))
    };
}
