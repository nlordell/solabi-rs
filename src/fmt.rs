//! Formatting helpers.

use std::fmt::{self, Debug, Display, Formatter};

/// A hexadecimal formater for byte slices.
pub struct Hex<'a>(pub &'a [u8]);

impl Debug for Hex<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for Hex<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("0x")?;
        for b in self.0 {
            write!(f, "{b:02x}")?;
        }
        Ok(())
    }
}
