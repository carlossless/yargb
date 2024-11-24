use cpu::CPU;
use registers::Flag::{C, H, N, Z};

macro_rules! rlc {
    ($source_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = cpu.rotate_left_carry(x);
            cpu.regs.$source_register = r;
            2
        }
    };
}

macro_rules! rlc_hl {
    () => {
        |cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            let x = cpu.mmu.read_byte(addr);
            let r = cpu.rotate_left_carry(x);
            cpu.mmu.write_byte(addr, r);
            4
        }
    };
}

macro_rules! rrc {
    ($source_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = cpu.rotate_right_carry(x);
            cpu.regs.$source_register = r;
            2
        }
    };
}

macro_rules! rrc_hl {
    () => {
        |cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            let x = cpu.mmu.read_byte(addr);
            let r = cpu.rotate_right_carry(x);
            cpu.mmu.write_byte(addr, r);
            4
        }
    };
}

macro_rules! rl {
    ($source_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = cpu.rotate_left(x);
            cpu.regs.$source_register = r;
            2
        }
    };
}

macro_rules! rl_hl {
    () => {
        |cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            let x = cpu.mmu.read_byte(addr);
            let r = cpu.rotate_left(x);
            cpu.mmu.write_byte(addr, r);
            4
        }
    };
}

macro_rules! rr {
    ($source_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = cpu.rotate_right(x);
            cpu.regs.$source_register = r;
            2
        }
    };
}

macro_rules! rr_hl {
    () => {
        |cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            let x = cpu.mmu.read_byte(addr);
            let r = cpu.rotate_right(x);
            cpu.mmu.write_byte(addr, r);
            4
        }
    };
}

macro_rules! srl {
    ($source_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = cpu.shift_right_logical(x);
            cpu.regs.$source_register = r;
            2
        }
    };
}

macro_rules! srl_hl {
    () => {
        |cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            let x = cpu.mmu.read_byte(addr);
            let r = cpu.shift_right_logical(x);
            cpu.mmu.write_byte(addr, r);
            4
        }
    };
}

macro_rules! swap {
    ($source_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = cpu.swap(x);
            cpu.regs.$source_register = r;
            2
        }
    };
}

macro_rules! swap_hl {
    () => {
        |cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            let x = cpu.mmu.read_byte(addr);
            let r = cpu.swap(x);
            cpu.mmu.write_byte(addr, r);
            4
        }
    };
}

macro_rules! sla {
    ($source_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = cpu.sla(x);
            cpu.regs.$source_register = r;
            2
        }
    };
}

macro_rules! sla_hl {
    () => {
        |cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            let x = cpu.mmu.read_byte(addr);
            let r = cpu.sla(x);
            cpu.mmu.write_byte(addr, r);
            4
        }
    };
}

macro_rules! sra {
    ($source_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = cpu.sra(x);
            cpu.regs.$source_register = r;
            2
        }
    };
}

macro_rules! sra_hl {
    () => {
        |cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            let x = cpu.mmu.read_byte(addr);
            let r = cpu.sra(x);
            cpu.mmu.write_byte(addr, r);
            4
        }
    };
}

macro_rules! bit {
    ($i:expr,$source_register:ident) => {
        |cpu: &mut CPU| {
            use registers::Flag::{H, N, Z};
            let x = cpu.regs.$source_register;
            cpu.regs.set_flag(Z, (x & (1 << $i)) == 0);
            cpu.regs.set_flag(N, false);
            cpu.regs.set_flag(H, true);
            2
        }
    };
}

macro_rules! bit_hl {
    ($i:expr) => {
        |cpu: &mut CPU| {
            use registers::Flag::{H, N, Z};
            let addr = cpu.regs.get_hl();
            let x = cpu.mmu.read_byte(addr);
            cpu.regs.set_flag(Z, (x & (1 << $i)) == 0);
            cpu.regs.set_flag(N, false);
            cpu.regs.set_flag(H, true);
            4
        }
    };
}

macro_rules! res {
    ($i:expr,$source_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = x & !(1 << $i);
            cpu.regs.$source_register = r;
            2
        }
    };
}

macro_rules! res_hl {
    ($i:expr) => {
        |cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            let x = cpu.mmu.read_byte(addr);
            cpu.mmu.write_byte(addr, x & !(1 << $i));
            4
        }
    };
}

macro_rules! set {
    ($i:expr,$source_register:ident) => {
        |cpu: &mut CPU| {
            let x = cpu.regs.$source_register;
            let r = x | (1 << $i);
            cpu.regs.$source_register = r;
            2
        }
    };
}

macro_rules! set_hl {
    ($i:expr) => {
        |cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            let x = cpu.mmu.read_byte(addr);
            cpu.mmu.write_byte(addr, x | (1 << $i));
            4
        }
    };
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
        self.regs.a = (a << 1) | (if c { 1 } else { 0 });
        1
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
        self.regs.a = (a >> 1) | (if c { 1 << 7 } else { 0 });
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

    pub fn rotate_left_carry(&mut self, value: u8) -> u8 {
        let c = value & (1 << 7) != 0;
        let r = value.rotate_left(1);
        self.regs.set_flag(Z, r == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, c);
        r
    }

    pub fn rotate_right_carry(&mut self, value: u8) -> u8 {
        let c = value & (1 << 0) != 0;
        let r = value.rotate_right(1);
        self.regs.set_flag(Z, r == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, c);
        r
    }

    pub fn rotate_left(&mut self, value: u8) -> u8 {
        let c = if self.regs.get_flag(C) { 1 } else { 0 };
        let r = (value << 1) | c;
        self.regs.set_flag(Z, r == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, (value & 0x80) != 0);
        r
    }

    pub fn rotate_right(&mut self, value: u8) -> u8 {
        let c = if self.regs.get_flag(C) { 1 } else { 0 };
        let r = c << 7 | (value >> 1);
        self.regs.set_flag(Z, r == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, (value & 0x01) != 0);
        r
    }

    pub fn sla(&mut self, value: u8) -> u8 {
        let c = value & 0x80 != 0;
        let r = value << 1;
        self.regs.set_flag(Z, r == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, c);
        r
    }

    pub fn sra(&mut self, value: u8) -> u8 {
        let c = value & 0x01 != 0;
        let r = (value & 0x80) | (value >> 1);
        self.regs.set_flag(Z, r == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, c);
        r
    }
}
