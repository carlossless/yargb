#[derive(Copy, Clone, Debug)]
pub struct Registers {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    f: u8,
    pub sp: u16,
    pub pc: u16,
}

#[repr(u8)]
pub enum Flag {
    Z = 1 << 7,
    N = 1 << 6,
    H = 1 << 5,
    C = 1 << 4
}

pub enum WordRegister {
    AF,
    BC,
    DE,
    HL,
    SP
}

impl Registers {
    pub fn new() -> Registers {
        Registers {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            f: 0,
            sp: 0,
            pc: 0x0100 // rom instruction start
        }
    }

    pub fn get_af(&mut self) -> u16 {
        ((self.a as u16) << 8) | (self.f as u16)
    }

    pub fn get_bc(&mut self) -> u16 {
        ((self.b as u16) << 8) | (self.c as u16)
    }

    pub fn get_de(&mut self) -> u16 {
        ((self.d as u16) << 8) | (self.e as u16)
    }

    pub fn get_hl(&mut self) -> u16 {
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
            WordRegister::AF => self.set_af(value),
            WordRegister::BC => self.set_bc(value),
            WordRegister::DE => self.set_de(value),
            WordRegister::HL => self.set_hl(value),
            WordRegister::SP => self.sp = value
        };
    }

    pub fn get(&mut self, register: WordRegister) -> u16 {
        match register {
            WordRegister::AF => self.get_af(),
            WordRegister::BC => self.get_bc(),
            WordRegister::DE => self.get_de(),
            WordRegister::HL => self.get_hl(),
            WordRegister::SP => self.sp
        }
    }

    pub fn set_flag(&mut self, mask: Flag, value: bool) {
        if value {
            self.f |= mask as u8
        } else {
            self.f &= !(mask as u8)
        }
    }

    pub fn get_flag(&mut self, mask: Flag) -> bool {
        (self.f & (mask as u8)) != 0
    }

}
