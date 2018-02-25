use num::Signed;

use std::fmt::{self, Formatter, UpperHex};

pub struct FormatAsSigned<T: Signed>(pub T);

impl<T: Signed + UpperHex + PartialOrd> UpperHex for FormatAsSigned<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let prefix = if f.alternate() { "0x" } else { "" };
        let bare_hex = format!("{:X}", self.0.abs());
        f.pad_integral(self.0 >= T::zero(), prefix, &bare_hex)
    }
}
