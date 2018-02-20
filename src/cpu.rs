use registers::Registers;
use mmu::MMU;

pub struct CPU {
	regs: Registers,
	mmu: MMU,
	pub halted: bool
}

impl CPU {
    pub fn new(rom_data: &[u8]) -> CPU {
        CPU {
        	regs: Registers::new(),
        	mmu: MMU::new(rom_data),
        	halted: false
        }
	}

	pub fn cycle(&mut self) -> usize {
		self.process()
	}

	fn process(&mut self) -> usize {
		let op = self.fetch_byte();
		match op {
			0x00 => { 1 }  // NOP
			0x01 => { let v = self.fetch_word(); self.regs.set_bc(v); 3 } // LD BC,nn
			0x02 => { self.mmu.write_byte(self.regs.get_bc(), self.regs.a); 2 } // LD (BC),A
			0x03 => { let mut v = self.regs.get_bc(); v = self.inc_word(v); self.regs.set_bc(v); 2 } // INC BC
			0x04 => { let mut v = self.regs.b; v = self.inc_byte(v); self.regs.b = v; 1 } // INC B
			0x05 => { let mut v = self.regs.b; v = self.dec_byte(v); self.regs.b = v; 1 } // DEC B
			0x06 => { let v = self.fetch_byte(); self.regs.b = v; 2 }

			0x13 => { let mut v = self.regs.get_de(); v = self.inc_word(v); self.regs.set_de(v); 2 } // INC DE
			0x14 => { let mut v = self.regs.d; v = self.inc_byte(v); self.regs.d = v; 1 } // INC D
			0x15 => { let mut v = self.regs.d; v = self.dec_byte(v); self.regs.d = v; 1 } // DEC D
			
			0x23 => { let mut v = self.regs.get_hl(); v = self.inc_word(v); self.regs.set_hl(v); 2 } // INC HL
			0x24 => { let mut v = self.regs.h; v = self.inc_byte(v); self.regs.h = v; 1 } // INC H
			0x25 => { let mut v = self.regs.h; v = self.dec_byte(v); self.regs.h = v; 1 } // DEC H
			
			0x33 => { let mut v = self.regs.sp; v = self.inc_word(v); self.regs.sp = v; 2 } // INC SP
			
			other => panic!("op {:2X} has not been implemented.", other)
		}
	}

	fn fetch_byte(&mut self) -> u8 {
		let b = self.mmu.read_byte(self.regs.pc);
		self.regs.pc += 1;
		b
	}

	fn fetch_word(&mut self) -> u16 {
		let w = self.mmu.read_word(self.regs.pc);
		self.regs.pc += 2;
		w
	}

	// ALU

	fn inc_byte(&mut self, mut value: u8) -> u8 {
		value + 1
		// TODO: Implement flags
	}

	fn inc_word(&mut self, mut value: u16) -> u16 {
		value + 1
		// TODO: Implement flags
	}

	fn dec_byte(&mut self, mut value: u8) -> u8 {
		value - 1
		// TODO: Implement flags
	}

	fn dec_word(&mut self, mut value: u16) -> u16 {
		value - 1
		// TODO: Implement flags
	}

	// SHIFTS

	fn rlca(&mut self, mut value: u16) -> u16 {
		value << 1
		// TODO: Implement flags
	}

	pub fn print_reg_state(&self) {
		println!("Hello, world! {:?}", self.regs);
	}
}
