use cpu::CPU;
use registers::Flag::{ Z, C };

impl CPU {
    pub fn jump_nz(&mut self, addr:u16) -> usize {
        if !self.regs.get_flag(Z) {
            return self.jump(addr)
        }
        2
    }

    pub fn jump_nc(&mut self, addr:u16) -> usize {
        if !self.regs.get_flag(C) {
            return self.jump(addr)
        }
        2
    }

    pub fn jump_z(&mut self, addr:u16) -> usize {
        if self.regs.get_flag(Z) {
            return self.jump(addr)
        }
        2
    }

    pub fn jump_c(&mut self, addr:u16) -> usize {
        if self.regs.get_flag(C) {
            return self.jump(addr)
        }
        2
    }

    pub fn jump(&mut self, addr:u16) -> usize {
        self.regs.pc = addr;
        4
    }

    pub fn relative_jump_nz(&mut self, addr:i8) -> usize {
        if !self.regs.get_flag(Z) {
            return self.relative_jump(addr)
        }
        2
    }

    pub fn relative_jump_nc(&mut self, addr:i8) -> usize {
        if !self.regs.get_flag(C) {
            return self.relative_jump(addr)
        }
        2
    }

    pub fn relative_jump_z(&mut self, addr:i8) -> usize {
        if self.regs.get_flag(Z) {
            return self.relative_jump(addr)
        }
        2
    }

    pub fn relative_jump_c(&mut self, addr:i8) -> usize {
        if self.regs.get_flag(C) {
            return self.relative_jump(addr)
        }
        2
    }

    pub fn relative_jump(&mut self, addr:i8) -> usize {
        self.regs.pc = (self.regs.pc as i16 + addr as i16) as u16;
        3
    } 
}