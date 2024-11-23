use std::result;

use cpu::CPU;
use registers::Flag::{C, H, N, Z};

macro_rules! alu_inc_byte {
    ($register:ident) => {
        |cpu: &mut CPU| {
            let mut v = cpu.regs.$register;
            cpu.alu_inc_byte(&mut v);
            cpu.regs.$register = v;
            1
        }
    };
}

macro_rules! alu_dec_byte {
    ($register:ident) => {
        |cpu: &mut CPU| {
            let mut v = cpu.regs.$register;
            cpu.alu_dec_byte(&mut v);
            cpu.regs.$register = v;
            1
        }
    };
}

// TODO: could be replaced by patern matched functions instead of the intermal match
macro_rules! alu_inc_word {
    ($register:ident) => {
        |cpu: &mut CPU| {
            let mut v = cpu.regs.get($register);
            cpu.alu_inc_word(&mut v);
            cpu.regs.set($register, v);
            2
        }
    };
}

// TODO: could be replaced by patern matched functions instead of the intermal match
macro_rules! alu_dec_word {
    ($register:ident) => {
        |cpu: &mut CPU| {
            let mut v = cpu.regs.get($register);
            cpu.alu_dec_word(&mut v);
            cpu.regs.set($register, v);
            2
        }
    };
}

macro_rules! alu_add {
    ($source_register:ident, $target_register:ident) => {
        |cpu: &mut CPU| {
            let addend = cpu.regs.$target_register;
            let v = cpu.regs.$source_register;
            let r = cpu.alu_add_byte(v, addend);
            cpu.regs.$source_register = r;
            1
        }
    };
}

macro_rules! alu_adc {
    ($source_register:ident, $target_register:ident) => {
        |cpu: &mut CPU| {
            let addend = cpu.regs.$target_register;
            let v = cpu.regs.$source_register;
            let r = cpu.alu_add_byte_with_carry(v, addend);
            cpu.regs.$source_register = r;
            1
        }
    };
}

macro_rules! alu_sub {
    ($source_register:ident, $target_register:ident) => {
        |cpu: &mut CPU| {
            let addend = cpu.regs.$target_register;
            let v = cpu.regs.$source_register;
            let r = cpu.alu_sub_byte(v, addend);
            cpu.regs.$source_register = r;
            1
        }
    };
}

macro_rules! alu_sbc {
    ($source_register:ident, $target_register:ident) => {
        |cpu: &mut CPU| {
            let addend = cpu.regs.$target_register;
            let v = cpu.regs.$source_register;
            let r = cpu.alu_sub_byte_with_carry(v, addend);
            cpu.regs.$source_register = r;
            1
        }
    };
}

macro_rules! alu_and {
    ($source_register:ident, $target_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let y = cpu.regs.$target_register;
            let r = cpu.alu_and(x, y);
            cpu.regs.$source_register = r;
            1
        }
    };
}

macro_rules! alu_xor {
    ($source_register:ident, $target_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let y = cpu.regs.$target_register;
            let r = cpu.alu_xor(x, y);
            cpu.regs.$source_register = r;
            1
        }
    };
}

macro_rules! alu_or {
    ($source_register:ident, $target_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let y = cpu.regs.$target_register;
            let r = cpu.alu_or(x, y);
            cpu.regs.$source_register = r;
            1
        }
    };
}

macro_rules! alu_cp {
    ($source_register:ident, $target_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let y = cpu.regs.$target_register;
            cpu.alu_cp(x, y);
            1
        }
    };
}

impl CPU {
    pub fn alu_inc_byte(&mut self, value: &mut u8) {
        let result = value.wrapping_add(1);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, (*value & 0x0F) + 1 > 0x0F);
        *value = result;
    }

    pub fn alu_inc_word(&mut self, value: &mut u16) {
        *value = value.wrapping_add(1)
    }

    pub fn alu_dec_byte(&mut self, value: &mut u8) {
        let result = value.wrapping_sub(1);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, true);
        self.regs.set_flag(H, (*value & 0x0F) == 0x0F);
        *value = result;
    }

    pub fn alu_dec_word(&mut self, value: &mut u16) {
        *value = value.wrapping_sub(1)
    }

    pub fn alu_add_to_hl(&mut self, value: u16) {
        let hl = self.regs.get_hl();
        let r = self.alu_add_word(hl, value);
        self.regs.set_hl(r);
    }

    pub fn alu_add_to_sp(&mut self, value: i8) -> u16 {
        let delta = value as i16 as u16;
        let sp = self.regs.sp;
        self.regs.set_flag(Z, false);
        self.regs.set_flag(N, false);
        self.regs
            .set_flag(H, (sp & 0x000F) + (delta & 0x000F) > 0x000F);
        self.regs
            .set_flag(C, (sp & 0x00FF) + (delta & 0x00FF) > 0x00FF);
        sp.wrapping_add(delta)
    }

    pub fn alu_add_word(&mut self, lhs: u16, rhs: u16) -> u16 {
        let result = lhs.wrapping_add(rhs);
        self.regs.set_flag(N, false);
        self.regs
            .set_flag(H, ((lhs & 0x07FF) + (rhs & 0x07FF)) > 0x07FF);
        self.regs.set_flag(C, lhs > 0xFFFF - rhs);
        result
    }

    pub fn alu_add_byte(&mut self, lhs: u8, rhs: u8) -> u8 {
        let result = lhs.wrapping_add(rhs);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, ((lhs & 0x0F) + (rhs & 0x0F)) > 0x0F);
        self.regs.set_flag(C, lhs > 0xFF - rhs);
        result
    }

    pub fn alu_add_byte_with_carry(&mut self, lhs: u8, rhs: u8) -> u8 {
        let carry = if self.regs.get_flag(C) { 1 } else { 0 };
        let result = lhs.wrapping_add(rhs).wrapping_add(carry);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs
            .set_flag(H, ((lhs & 0x0F) + (rhs & 0x0F)).wrapping_add(carry) > 0x0F);
        let carria = if 0xFF - rhs != 0 {
            lhs > 0xFF - rhs - carry
        } else {
            carry == 1
        };
        self.regs.set_flag(C, carria);

        result
    }

    pub fn alu_sub_byte(&mut self, lhs: u8, rhs: u8) -> u8 {
        let result = lhs.wrapping_sub(rhs);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, true);
        self.regs.set_flag(H, (lhs & 0x0F) < (rhs & 0x0F));
        self.regs.set_flag(C, lhs < rhs);
        result
    }

    pub fn alu_sub_byte_with_carry(&mut self, lhs: u8, rhs: u8) -> u8 {
        let carry = if self.regs.get_flag(C) { 1 } else { 0 };
        let result = lhs.wrapping_add(rhs).wrapping_add(carry);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, true);
        self.regs
            .set_flag(H, (lhs & 0x0F) < (rhs & 0x0F).wrapping_add(carry));
        self.regs
            .set_flag(C, (lhs as u16) < (rhs as u16) + (carry as u16));
        result
    }

    pub fn alu_and(&mut self, lhs: u8, rhs: u8) -> u8 {
        let result = lhs & rhs;
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, true);
        self.regs.set_flag(C, false);
        result
    }

    pub fn alu_xor(&mut self, lhs: u8, rhs: u8) -> u8 {
        let result = lhs ^ rhs;
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, false);
        result
    }

    pub fn alu_or(&mut self, lhs: u8, rhs: u8) -> u8 {
        let result = lhs | rhs;
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, false);
        result
    }

    pub fn alu_cp(&mut self, lhs: u8, rhs: u8) {
        self.alu_sub_byte(lhs, rhs);
    }

    pub fn daa(&mut self) -> usize {
        let n = self.regs.get_flag(N);
        let h = self.regs.get_flag(H);
        let mut c = self.regs.get_flag(C);
        let mut work = self.regs.a as u16;

        if n {
            if h {
                work = work.wrapping_sub(0x06) & 0xff;
            }
            if c {
                work = work.wrapping_sub(0x60);
            }
        } else {
            if h || (work & 0x0F) > 9 {
                work = work.wrapping_add(0x06);
            }
            if c || work > 0x9F {
                work = work.wrapping_add(0x60);
            }
        }

        if (work & 0x100) == 0x100 {
            c = true;
        }

        self.regs.a = (work & 0xff) as u8;
        self.regs.set_flag(Z, self.regs.a == 0);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, c);
        1
    }

    pub fn alu_complement(&mut self) -> usize {
        self.regs.set_flag(N, true);
        self.regs.set_flag(H, true);
        self.regs.a = !self.regs.a;
        1
    }

    pub fn alu_set_carry_flag(&mut self) -> usize {
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, true);
        1
    }

    pub fn alu_complement_carry_flag(&mut self) -> usize {
        let c = !self.regs.get_flag(C);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, c);
        1
    }
}
