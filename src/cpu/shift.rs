use cpu::CPU;
use registers::Flag::{ Z, N, H, C };

macro_rules! rotate_right {
    ($source_register:ident) => (
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = cpu.rotate_right(x);
            cpu.regs.$source_register = r;
            2
        }
    )
}

macro_rules! shift_right_logical {
    ($source_register:ident) => (
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = cpu.shift_right_logical(x);
            cpu.regs.$source_register = r;
            2
        }
    )
}

macro_rules! swap {
    ($source_register:ident) => (
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = cpu.swap(x);
            cpu.regs.$source_register = r;
            2
        }
    )
}

impl CPU {

    pub fn rotate_left_circular_accumulator(&mut self) -> usize {
        let a = self.regs.a;
        self.regs.set_flag(Z, false);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, (a & (1 << 7)) != 0);
        self.regs.a = a.rotate_left(1);
        1
    }

    pub fn rotate_left_accumulator(&mut self) -> usize {
        let a = self.regs.a;
        let c = self.regs.get_flag(C);
        self.regs.set_flag(Z, false);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, (a & (1 << 7)) != 0);
        self.regs.a = a << 1 | (if c { 1 } else { 0 });
        1
    }

    pub fn rotate_right(&mut self, value: u8) -> u8 {
        let r = value.rotate_right(1);
        self.regs.set_flag(Z, r == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, (value & 0x01) != 0);
        r
    }

    pub fn rotate_right_circular_accumulator(&mut self) -> usize {
        let a = self.regs.a;
        self.regs.set_flag(Z, false);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, (a & 1) != 0);
        self.regs.a = a.rotate_right(1);
        1
    }

    pub fn rotate_right_accumulator(&mut self) -> usize {
        let a = self.regs.a;
        let c = self.regs.get_flag(C);
        self.regs.set_flag(Z, false);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, (a & 1) != 0);
        self.regs.a = a >> 1 | (if c { 1 << 7 } else { 0 });
        1
    }

    pub fn shift_right_logical(&mut self, value: u8) -> u8 {
        let c = value & 0x01 == 0x01;
        let r = value >> 1;
        self.regs.set_flag(Z, r == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, c);
        r
    }

    pub fn swap(&mut self, value: u8) -> u8 {
        let r = value >> 4 | value << 4;
        self.regs.set_flag(Z, r == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, false);
        r
    }

}