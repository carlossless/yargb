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
			0x06 => { let v = self.fetch_byte(); self.regs.b = v; 2 } // LD B,d8
			// RLCA
			// LD (a16),SP
			// ADD HL,BC
			// LD A,(BC)
			0x0B => { let mut v = self.regs.get_bc(); v = self.dec_word(v); self.regs.set_bc(v); 2 } // DEC BC
			0x0C => { let mut v = self.regs.c; v = self.inc_byte(v); self.regs.c = v; 1 } // INC C
			0x0D => { let mut v = self.regs.c; v = self.dec_byte(v); self.regs.c = v; 1 } // DEC C
			0x0E => { let v = self.fetch_byte(); self.regs.c = v; 2 } // LD C,d8
			// RRCA

			// STOP 0
			0x11 => { let mut v = self.fetch_word(); self.regs.set_de(v); 3 } // LD DE,d16
			0x12 => { self.mmu.write_byte(self.regs.get_de(), self.regs.a); 3 } // LD (DE),A
			0x13 => { let mut v = self.regs.get_de(); v = self.inc_word(v); self.regs.set_de(v); 2 } // INC DE
			0x14 => { let mut v = self.regs.d; v = self.inc_byte(v); self.regs.d = v; 1 } // INC D
			0x15 => { let mut v = self.regs.d; v = self.dec_byte(v); self.regs.d = v; 1 } // DEC D
			0x16 => { let v = self.fetch_byte(); self.regs.d = v; 2 } // LD D,d8
			// RLA
			// JR r8
			// ADD HL,DE
			// LD A,(DE)
			0x1B => { let mut v = self.regs.get_de(); v = self.dec_word(v); self.regs.set_de(v); 2 } // DEC DE
			0x1C => { let mut v = self.regs.e; v = self.inc_byte(v); self.regs.e = v; 1 } // INC E
			0x1D => { let mut v = self.regs.e; v = self.dec_byte(v); self.regs.e = v; 1 } // DEC E
			0x1E => { let v = self.fetch_byte(); self.regs.e = v; 2 } // LD E,d8
			// RRA
			
			// JR NZ, r8
			0x21 => { let mut v = self.fetch_word(); self.regs.set_hl(v); 3 } // LD HL,d16
			// LD (HL+),A
			0x23 => { let mut v = self.regs.get_hl(); v = self.inc_word(v); self.regs.set_hl(v); 2 } // INC HL
			0x24 => { let mut v = self.regs.h; v = self.inc_byte(v); self.regs.h = v; 1 } // INC H
			0x25 => { let mut v = self.regs.h; v = self.dec_byte(v); self.regs.h = v; 1 } // DEC H
			0x26 => { let v = self.fetch_byte(); self.regs.h = v; 2 } // LD H,d8
			// DAA
			// JR Z,r8
			// ADD HL,HL
			// LD A,(HL+)
			0x2B => { let mut v = self.regs.get_hl(); v = self.dec_word(v); self.regs.set_hl(v); 2 } // DEC HL
			0x2C => { let mut v = self.regs.l; v = self.inc_byte(v); self.regs.l = v; 1 } // INC L
			0x2D => { let mut v = self.regs.l; v = self.dec_byte(v); self.regs.l = v; 1 } // DEC L
			0x2E => { let v = self.fetch_byte(); self.regs.l = v; 2 } // LD E,d8
			// CPL
			
			// JR NC,r8
			0x31 => { let mut v = self.fetch_word(); self.regs.sp = v; 3 } // LD SP,d16
			// LD (HL-),A
			0x33 => { let mut v = self.regs.sp; v = self.inc_word(v); self.regs.sp = v; 2 } // INC SP
			// INC (HL)
			// DEC (HL)
			// LD (HL),d8
			// SCF
			// JR C,r8
			// ADD HL,SP
			// LD A,(HL-)
			0x3B => { let mut v = self.regs.sp; v = self.dec_word(v); self.regs.sp = v; 2 } // DEC SP
			0x3C => { let mut v = self.regs.a; v = self.inc_byte(v); self.regs.a = v; 1 } // INC A
			0x3D => { let mut v = self.regs.a; v = self.dec_byte(v); self.regs.a = v; 1 } // DEC A
			0x3E => { let v = self.fetch_byte(); self.regs.a = v; 2 } // LD A,d8
			// CCF
			
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
		let result = value.wrapping_add(1);
		self.regs.set_flag(1 << 7, result == 0); // Z
		self.regs.set_flag(1 << 6, false); // N
		self.regs.set_flag(1 << 5, (value & 0x0F) + 1 > 0x0F); // H
		result
	}

	fn inc_word(&mut self, mut value: u16) -> u16 {
		value.wrapping_add(1)
	}

	fn dec_byte(&mut self, mut value: u8) -> u8 {
		let result = value.wrapping_sub(1);
		self.regs.set_flag(1 << 7, result == 0); // Z
		self.regs.set_flag(1 << 6, true); // N
		self.regs.set_flag(1 << 5, (value & 0x0F) == 0x0F); // H
		result
	}

	fn dec_word(&mut self, mut value: u16) -> u16 {
		value.wrapping_sub(1)
	}

	// DEBUG

	pub fn print_reg_state(&self) {
		println!("Hello, world! {:?}", self.regs);
	}
}
