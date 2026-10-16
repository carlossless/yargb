#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Registers {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub(crate) f: u8,
    pub sp: u16,
    pub pc: u16,

    pub ime: bool,
}

#[repr(u8)]
pub enum Flag {
    Z = 1 << 7,
    N = 1 << 6,
    H = 1 << 5,
    C = 1 << 4,
}

pub enum WordRegister {
    BC,
    DE,
    HL,
    SP,
}

pub const DEFAULT_DMG_REGISTERS: Registers = Registers {
    a: 0x01,
    b: 0x00,
    c: 0x13,
    d: 0x00,
    e: 0xD8,
    h: 0x01,
    l: 0x4D,
    f: 0xB0,
    sp: 0xFFFE, // top of ram
    pc: 0x0100, // rom instruction start
    ime: false,
};

/// The registers the CGB boot ROM leaves for a CGB game. Games check A to
/// detect a CGB.
pub const DEFAULT_CGB_REGISTERS: Registers = Registers {
    a: 0x11,
    b: 0x00,
    c: 0x00,
    d: 0xFF,
    e: 0x56,
    h: 0x00,
    l: 0x0D,
    f: 0x80,
    sp: 0xFFFE,
    pc: 0x0100,
    ime: false,
};

/// The registers the CGB boot ROM leaves when it runs a DMG cartridge.
pub const DEFAULT_CGB_COMPAT_REGISTERS: Registers = Registers {
    d: 0x00,
    e: 0x08,
    l: 0x7C,
    ..DEFAULT_CGB_REGISTERS
};

impl Registers {
    pub fn get_af(&self) -> u16 {
        ((self.a as u16) << 8) | (self.f as u16)
    }

    pub fn get_bc(&self) -> u16 {
        ((self.b as u16) << 8) | (self.c as u16)
    }

    pub fn get_de(&self) -> u16 {
        ((self.d as u16) << 8) | (self.e as u16)
    }

    pub fn get_hl(&self) -> u16 {
        ((self.h as u16) << 8) | (self.l as u16)
    }

    pub fn set_af(&mut self, value: u16) {
        self.a = (value >> 8) as u8;
        self.f = (value & 0xff) as u8;
    }

    pub fn set_bc(&mut self, value: u16) {
        self.b = (value >> 8) as u8;
        self.c = (value & 0xff) as u8;
    }

    pub fn set_de(&mut self, value: u16) {
        self.d = (value >> 8) as u8;
        self.e = (value & 0xff) as u8;
    }

    pub fn set_hl(&mut self, value: u16) {
        self.h = (value >> 8) as u8;
        self.l = (value & 0xff) as u8;
    }

    pub fn set(&mut self, register: WordRegister, value: u16) {
        match register {
            WordRegister::BC => self.set_bc(value),
            WordRegister::DE => self.set_de(value),
            WordRegister::HL => self.set_hl(value),
            WordRegister::SP => self.sp = value,
        };
    }

    pub fn get(&self, register: WordRegister) -> u16 {
        match register {
            WordRegister::BC => self.get_bc(),
            WordRegister::DE => self.get_de(),
            WordRegister::HL => self.get_hl(),
            WordRegister::SP => self.sp,
        }
    }

    pub fn set_flag(&mut self, mask: Flag, value: bool) {
        if value {
            self.f |= mask as u8
        } else {
            self.f &= !(mask as u8)
        }
    }

    pub fn get_flag(&self, mask: Flag) -> bool {
        (self.f & (mask as u8)) != 0
    }

    pub fn one_line_rep(&self) -> String {
        format!(
            "A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:04X} PC:{:04X} Z:{} N:{} H:{} C:{}",
            self.a,
            self.f,
            self.b,
            self.c,
            self.d,
            self.e,
            self.h,
            self.l,
            self.sp,
            self.pc,
            self.get_flag(Flag::Z),
            self.get_flag(Flag::N),
            self.get_flag(Flag::H),
            self.get_flag(Flag::C)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pairs_combine_high_and_low_registers() {
        let mut registers = DEFAULT_DMG_REGISTERS;
        registers.set_bc(0x1234);
        registers.set_de(0x5678);
        registers.set_hl(0x9ABC);
        assert_eq!((registers.b, registers.c), (0x12, 0x34));
        assert_eq!((registers.d, registers.e), (0x56, 0x78));
        assert_eq!((registers.h, registers.l), (0x9A, 0xBC));
        registers.set(WordRegister::SP, 0xFFFE);
        assert_eq!(registers.get(WordRegister::SP), 0xFFFE);
        assert_eq!(registers.get(WordRegister::DE), 0x5678);
    }

    #[test]
    fn flags_are_the_top_bits_of_f() {
        let mut registers = DEFAULT_DMG_REGISTERS;
        registers.set_af(0x0000);
        registers.set_flag(Flag::Z, true);
        registers.set_flag(Flag::C, true);
        assert_eq!(registers.get_af(), 0x0090);
        assert!(registers.get_flag(Flag::Z) && !registers.get_flag(Flag::N));
        registers.set_flag(Flag::Z, false);
        assert_eq!(registers.f, 0x10);
    }
}
