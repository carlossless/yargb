use cpu::CPU;
use registers::Flag::{C, Z};

macro_rules! stack_rst {
    ($addr:expr) => {
        |cpu: &mut CPU| cpu.stack_rst($addr)
    };
}

impl CPU {
    pub fn stack_ret_nz(&mut self) -> usize {
        if !self.regs.get_flag(Z) {
            self.regs.pc = self.stack_pop();
            return 5;
        }
        2
    }

    pub fn stack_ret_nc(&mut self) -> usize {
        if !self.regs.get_flag(C) {
            self.regs.pc = self.stack_pop();
            return 5;
        }
        2
    }

    pub fn stack_ret_z(&mut self) -> usize {
        if self.regs.get_flag(Z) {
            self.regs.pc = self.stack_pop();
            return 5;
        }
        2
    }

    pub fn stack_ret_c(&mut self) -> usize {
        if self.regs.get_flag(C) {
            self.regs.pc = self.stack_pop();
            return 5;
        }
        2
    }

    pub fn stack_ret(&mut self) -> usize {
        self.regs.pc = self.stack_pop();
        return 4;
    }

    pub fn stack_reti(&mut self) -> usize {
        self.regs.pc = self.stack_pop();
        self.regs.ime = false; // FIXME: should be set to the pre-interrupt value
        return 4;
    }

    pub fn stack_rst(&mut self, addr: u16) -> usize {
        self.stack_call(addr);
        4
    }

    pub fn stack_call_nz(&mut self, value: u16) -> usize {
        if !self.regs.get_flag(Z) {
            return self.stack_call(value);
        }
        3
    }

    pub fn stack_call_nc(&mut self, value: u16) -> usize {
        if !self.regs.get_flag(C) {
            return self.stack_call(value);
        }
        3
    }

    pub fn stack_call_z(&mut self, value: u16) -> usize {
        if self.regs.get_flag(Z) {
            return self.stack_call(value);
        }
        3
    }

    pub fn stack_call_c(&mut self, value: u16) -> usize {
        if self.regs.get_flag(C) {
            return self.stack_call(value);
        }
        3
    }

    pub fn stack_call(&mut self, value: u16) -> usize {
        let next_op = self.regs.pc; // value taken before hand
        self.stack_push(next_op);
        self.regs.pc = value;
        6
    }

    pub fn stack_pop(&mut self) -> u16 {
        let mut result = 0;
        result |= self.mmu.read_byte(self.regs.sp) as u16;
        self.regs.sp += 1;
        result |= (self.mmu.read_byte(self.regs.sp) as u16) << 8;
        self.regs.sp += 1;
        result
    }

    pub fn stack_push_af(&mut self) -> usize {
        let value = self.regs.get_af();
        self.stack_push(value);
        4
    }

    pub fn stack_push_bc(&mut self) -> usize {
        let value = self.regs.get_bc();
        self.stack_push(value);
        4
    }

    pub fn stack_push_de(&mut self) -> usize {
        let value = self.regs.get_de();
        self.stack_push(value);
        4
    }

    pub fn stack_push_hl(&mut self) -> usize {
        let value = self.regs.get_hl();
        self.stack_push(value);
        4
    }

    pub fn stack_push(&mut self, value: u16) {
        self.regs.sp -= 1;
        self.mmu.write_byte(self.regs.sp, ((value >> 8) & 0xff) as u8);
        self.regs.sp -= 1;
        self.mmu.write_byte(self.regs.sp, (value & 0xff) as u8);
    }
}
