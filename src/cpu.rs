use registers::Registers;
use registers::Flag::{ Z, N, H, C };
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
			0x07 => { self.rotate_left_circular_accumulator(); 1 } // RLCA
			0x08 => { let v = self.regs.sp; let a = self.fetch_word(); self.mmu.write_word(a, v); 5 } // LD (a16),SP
			0x09 => { let v = self.regs.get_bc(); self.add_to_hl(v); 2 } // ADD HL,BC
			0x0A => { let a = self.regs.get_bc(); self.regs.a = self.mmu.read_byte(a); 2 } // LD A,(BC)
			0x0B => { let mut v = self.regs.get_bc(); v = self.dec_word(v); self.regs.set_bc(v); 2 } // DEC BC
			0x0C => { let mut v = self.regs.c; v = self.inc_byte(v); self.regs.c = v; 1 } // INC C
			0x0D => { let mut v = self.regs.c; v = self.dec_byte(v); self.regs.c = v; 1 } // DEC C
			0x0E => { let v = self.fetch_byte(); self.regs.c = v; 2 } // LD C,d8
			0x0F => { self.rotate_right_circular_accumulator(); 1 } // RRCA

			// STOP 0
			0x11 => { let mut v = self.fetch_word(); self.regs.set_de(v); 3 } // LD DE,d16
			0x12 => { self.mmu.write_byte(self.regs.get_de(), self.regs.a); 3 } // LD (DE),A
			0x13 => { let mut v = self.regs.get_de(); v = self.inc_word(v); self.regs.set_de(v); 2 } // INC DE
			0x14 => { let mut v = self.regs.d; v = self.inc_byte(v); self.regs.d = v; 1 } // INC D
			0x15 => { let mut v = self.regs.d; v = self.dec_byte(v); self.regs.d = v; 1 } // DEC D
			0x16 => { let v = self.fetch_byte(); self.regs.d = v; 2 } // LD D,d8
			0x17 => { self.rotate_left_accumulator(); 1 } // RLA
			// JR r8
			0x19 => { let v = self.regs.get_de(); self.add_to_hl(v); 2 } // ADD HL,DE
			0x1A => { let a = self.regs.get_de(); self.regs.a = self.mmu.read_byte(a); 2 } // LD A,(DE)
			0x1B => { let mut v = self.regs.get_de(); v = self.dec_word(v); self.regs.set_de(v); 2 } // DEC DE
			0x1C => { let mut v = self.regs.e; v = self.inc_byte(v); self.regs.e = v; 1 } // INC E
			0x1D => { let mut v = self.regs.e; v = self.dec_byte(v); self.regs.e = v; 1 } // DEC E
			0x1E => { let v = self.fetch_byte(); self.regs.e = v; 2 } // LD E,d8
			0x1F => { self.rotate_right_accumulator(); 1 } // RRA
			
			// JR NZ, r8
			0x21 => { let mut v = self.fetch_word(); self.regs.set_hl(v); 3 } // LD HL,d16
			// LD (HL+),A
			0x23 => { let mut v = self.regs.get_hl(); v = self.inc_word(v); self.regs.set_hl(v); 2 } // INC HL
			0x24 => { let mut v = self.regs.h; v = self.inc_byte(v); self.regs.h = v; 1 } // INC H
			0x25 => { let mut v = self.regs.h; v = self.dec_byte(v); self.regs.h = v; 1 } // DEC H
			0x26 => { let v = self.fetch_byte(); self.regs.h = v; 2 } // LD H,d8
			0x27 => { self.decimal_adjust_accumulator(); 1 } // DAA
			// JR Z,r8
			0x29 => { let v = self.regs.get_hl(); self.add_to_hl(v); 2 } // ADD HL,HL
			// LD A,(HL+)
			0x2B => { let mut v = self.regs.get_hl(); v = self.dec_word(v); self.regs.set_hl(v); 2 } // DEC HL
			0x2C => { let mut v = self.regs.l; v = self.inc_byte(v); self.regs.l = v; 1 } // INC L
			0x2D => { let mut v = self.regs.l; v = self.dec_byte(v); self.regs.l = v; 1 } // DEC L
			0x2E => { let v = self.fetch_byte(); self.regs.l = v; 2 } // LD E,d8
			0x2F => { self.complement(); 1 } // CPL
			
			// JR NC,r8
			0x31 => { let mut v = self.fetch_word(); self.regs.sp = v; 3 } // LD SP,d16
			// LD (HL-),A
			0x33 => { let mut v = self.regs.sp; v = self.inc_word(v); self.regs.sp = v; 2 } // INC SP
			0x34 => { let a = self.regs.get_hl(); let mut v = self.mmu.read_byte(a); v = self.inc_byte(v); self.mmu.write_byte(a, v); 3 } // INC (HL)
			0x35 => { let a = self.regs.get_hl(); let mut v = self.mmu.read_byte(a); v = self.dec_byte(v); self.mmu.write_byte(a, v); 3 } // DEC (HL)
			0x36 => { let a = self.regs.get_hl(); let v = self.fetch_byte(); self.mmu.write_byte(a, v); 3 } // LD (HL),d8
			0x37 => { self.set_carry_flag(); 1 } // SCF
			// JR C,r8
			0x39 => { let v = self.regs.sp; self.add_to_hl(v); 2 } // ADD HL,SP
			// LD A,(HL-)
			0x3B => { let mut v = self.regs.sp; v = self.dec_word(v); self.regs.sp = v; 2 } // DEC SP
			0x3C => { let mut v = self.regs.a; v = self.inc_byte(v); self.regs.a = v; 1 } // INC A
			0x3D => { let mut v = self.regs.a; v = self.dec_byte(v); self.regs.a = v; 1 } // DEC A
			0x3E => { let v = self.fetch_byte(); self.regs.a = v; 2 } // LD A,d8
			0x3F => { self.complement_carry_flag(); 1 } // CCF
			
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

	fn inc_byte(&mut self, value: u8) -> u8 {
		let result = value.wrapping_add(1);
		self.regs.set_flag(Z, result == 0);
		self.regs.set_flag(N, false);
		self.regs.set_flag(H, (value & 0x0F) + 1 > 0x0F);
		result
	}

	fn inc_word(&mut self, value: u16) -> u16 {
		value.wrapping_add(1)
	}

	fn dec_byte(&mut self, value: u8) -> u8 {
		let result = value.wrapping_sub(1);
		self.regs.set_flag(Z, result == 0);
		self.regs.set_flag(N, true);
		self.regs.set_flag(H, (value & 0x0F) == 0x0F);
		result
	}

	fn dec_word(&mut self, value: u16) -> u16 {
		value.wrapping_sub(1)
	}

	fn add_to_hl(&mut self, value: u16) {
		let hl = self.regs.get_hl();
		self.add_word(hl, value);
		self.regs.set_hl(hl);
	}

	fn add_word(&mut self, lhs: u16, rhs: u16) -> u16 {
		let result = lhs.wrapping_add(rhs);
		self.regs.set_flag(N, false);
		self.regs.set_flag(H, (lhs & 0x07FF) + (rhs & 0x07FF) > 0x07FF);
		self.regs.set_flag(C, lhs > 0xFFFF - rhs);
		result
	}

	// TODO: using match here is unncessary... can be done more concisely by mutating through conditions
	fn decimal_adjust_accumulator(&mut self) {
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
	}

	fn complement(&mut self) {
		self.regs.set_flag(N, true);
		self.regs.set_flag(H, true);
		self.regs.a = !self.regs.a;
	}

	fn set_carry_flag(&mut self) {
		self.regs.set_flag(N, false);
		self.regs.set_flag(H, false);
		self.regs.set_flag(C, true);
	}

	fn complement_carry_flag(&mut self) {
		let c = !self.regs.get_flag(C);
		self.regs.set_flag(N, false);
		self.regs.set_flag(H, false);
		self.regs.set_flag(C, c);
	}

	// Rotates

	fn rotate_left_circular_accumulator(&mut self) {
		let a = self.regs.a;
		self.regs.set_flag(Z, false);
		self.regs.set_flag(N, false);
		self.regs.set_flag(H, false);
		self.regs.set_flag(C, (a & (1 << 7)) != 0);
		self.regs.a = a.rotate_left(1);
	}

	fn rotate_left_accumulator(&mut self) {
		let a = self.regs.a;
		let c = self.regs.get_flag(C);
		self.regs.set_flag(Z, false);
		self.regs.set_flag(N, false);
		self.regs.set_flag(H, false);
		self.regs.set_flag(C, (a & (1 << 7)) != 0);
		self.regs.a = a << 1 | (if c { 1 } else { 0 });
	}

	fn rotate_right_circular_accumulator(&mut self) {
		let a = self.regs.a;
		self.regs.set_flag(Z, false);
		self.regs.set_flag(N, false);
		self.regs.set_flag(H, false);
		self.regs.set_flag(C, (a & 1) > 0);
		self.regs.a = a.rotate_right(1);
	}

	fn rotate_right_accumulator(&mut self) {
		let a = self.regs.a;
		let c = self.regs.get_flag(C);
		self.regs.set_flag(Z, false);
		self.regs.set_flag(N, false);
		self.regs.set_flag(H, false);
		self.regs.set_flag(C, (a & 1) != 0);
		self.regs.a = a >> 1 | (if c { 1 << 7 } else { 0 });
	}

	// DEBUG

	pub fn print_reg_state(&self) {
		println!("Hello, world! {:?}", self.regs);
	}
}
