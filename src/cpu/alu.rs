use cpu::CPU;
use registers::Flag::{ Z, N, H, C };

macro_rules! inc_byte {
    ($register:ident) => (
        |cpu: &mut CPU| {
            let mut v = cpu.regs.$register;
            cpu.alu_inc_byte(&mut v);
            cpu.regs.$register = v;
            1
        }
    )
}

macro_rules! dec_byte {
    ($register:ident) => (
        |cpu: &mut CPU| {
            let mut v = cpu.regs.$register;
            cpu.alu_dec_byte(&mut v);
            cpu.regs.$register = v;
            1
        }
    )
}

// TODO: could be replaced by patern matched functions instead of the intermal match
macro_rules! inc_word {
    ($register:ident) => (
        |cpu: &mut CPU| {
            let mut v = cpu.regs.get($register);
            cpu.alu_inc_word(&mut v);
            cpu.regs.set($register, v);
            2
        }
    )
}

// TODO: could be replaced by patern matched functions instead of the intermal match
macro_rules! dec_word {
    ($register:ident) => (
        |cpu: &mut CPU| {
            let mut v = cpu.regs.get($register);
            cpu.alu_dec_word(&mut v);
            cpu.regs.set($register, v);
            2
        }
    )
}

macro_rules! add {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            let addend = cpu.regs.$source_register;
            let mut v = cpu.regs.$target_register;
            v = cpu.alu_add_byte(v, addend);
            cpu.regs.$target_register = v;
            1
        }
    )
}

macro_rules! adc {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            let addend = cpu.regs.$source_register;
            let mut v = cpu.regs.$target_register;
            v = cpu.alu_add_byte_with_carry(v, addend);
            cpu.regs.$target_register = v;
            1
        }
    )
}

macro_rules! sub {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            let addend = cpu.regs.$source_register;
            let mut v = cpu.regs.$target_register;
            v = cpu.alu_sub_byte(v, addend);
            cpu.regs.$target_register = v;
            1
        }
    )
}

macro_rules! sbc {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            let addend = cpu.regs.$source_register;
            let mut v = cpu.regs.$target_register;
            v = cpu.alu_sub_byte_with_carry(v, addend);
            cpu.regs.$target_register = v;
            1
        }
    )
}

macro_rules! and {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let y = cpu.regs.$target_register;
            let r = cpu.alu_and(x, y);
            cpu.regs.$target_register = r;
            1
        }
    )
}

macro_rules! xor {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let y = cpu.regs.$target_register;
            let r = cpu.alu_xor(x, y);
            cpu.regs.$target_register = r;
            1
        }
    )
}

macro_rules! or {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let y = cpu.regs.$target_register;
            let r = cpu.alu_or(x, y);
            cpu.regs.$target_register = r;
            1
        }
    )
}

macro_rules! cp {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let y = cpu.regs.$target_register;
            cpu.alu_cp(x, y);
            1
        }
    )
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
        self.alu_add_word(hl, value);
        self.regs.set_hl(hl);
    }

    pub fn alu_add_word(&mut self, lhs: u16, rhs: u16) -> u16 {
        let result = lhs.wrapping_add(rhs);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, ((lhs & 0x07FF) + (rhs & 0x07FF)) > 0x07FF);
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
        self.regs.set_flag(H, ((lhs & 0x0F) + (rhs & 0x0F)).wrapping_add(carry) > 0x0F);
        self.regs.set_flag(C, lhs > 0xFF - rhs - carry);
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
        self.regs.set_flag(H, (lhs & 0x0F) < (rhs & 0x0F).wrapping_add(carry));
        self.regs.set_flag(C, (lhs as u16) < (rhs as u16) + (carry as u16));
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
        let result = lhs ^ rhs;
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, false);
        result
    }

    pub fn alu_cp(&mut self, lhs: u8, rhs: u8) {
        self.alu_sub_byte(lhs, rhs);
    }

    // TODO: using match here is unnecessary... can be done more concisely by mutating through conditions
    pub fn alu_decimal_adjust_accumulator(&mut self) -> usize {
        let n = self.regs.get_flag(N);
        let h = self.regs.get_flag(H);
        let mut c = self.regs.get_flag(C);
        let mut a = self.regs.a;
        let upper_digit = a >> 4;
        let lower_digit = a & 0xF;
        c = if n {
            match (c, h, upper_digit, lower_digit) {
                (false, false, 0x0...0x9, 0x0...0x9) => { false }
                (false, false, 0x0...0x8, 0xA...0xF) => { a = a.wrapping_add(0x06); false }
                (false, true,  0x0...0x9, 0x0...0x3) => { a = a.wrapping_add(0x06); false }
                (false, false, 0xA...0xF, 0x0...0x9) => { a = a.wrapping_add(0x60); true }
                (false, false, 0x9...0xF, 0xA...0xF) => { a = a.wrapping_add(0x66); true }
                (false, true , 0xA...0xF, 0x0...0x3) => { a = a.wrapping_add(0x66); true }
                (true , false, 0x0...0x2, 0x0...0x9) => { a = a.wrapping_add(0x60); true }
                (true , false, 0x0...0x2, 0xA...0xF) => { a = a.wrapping_add(0x66); true }
                (true , true , 0x0...0x3, 0x0...0x3) => { a = a.wrapping_add(0x66); true }
                _ => { panic!("DAA should never reach this case") }
            }
        } else {
            match (c, h, upper_digit, lower_digit) {
                (false, false, 0x0...0x9, 0x0...0x9) => { false }
                (false, true,  0x0...0x8, 0x6...0xF) => { a = a.wrapping_add(0xFA); false }
                (false, false, 0x9...0xF, 0xA...0xF) => { a = a.wrapping_add(0xA0); true }
                (true , true , 0xA...0xF, 0x0...0x3) => { a = a.wrapping_add(0x9A); true }
                _ => { panic!("DAA should never reach this case") }
            }
        };
        self.regs.set_flag(Z, a == 0);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, c);
        self.regs.a = a;
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

