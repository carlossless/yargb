use registers::Registers;
use registers::Flag::{ Z, N, H, C };
use registers::WordRegister::{ BC, DE, HL, SP };
use mmu::MMU;

pub struct CPU {
    regs: Registers,
    mmu: MMU,
    pub halted: bool
}

struct Operation {
    execute: &'static Fn(&mut CPU) -> usize
}

macro_rules! dop {
    ($name:tt, $execute:expr) => (Operation {
        execute: &|cpu| {
            println!($name);
            $execute(cpu)
        }
    });
    ($name:tt, u8, $execute:expr) => (Operation {
        execute: &|cpu: &mut CPU| {
            let value: u8 = cpu.fetch_byte();
            println!($name, value);
            $execute(cpu, value)
        }
    });
    ($name:tt, u16, $execute:expr) => (Operation {
        execute: &|cpu: &mut CPU| {
            let value: u16 = cpu.fetch_word();
            println!($name, value);
            $execute(cpu, value)
        }
    });
}

macro_rules! inc_byte {
    ($register:ident) => (
        |cpu: &mut CPU| { let mut v = cpu.regs.$register; cpu.inc_byte(&mut v); cpu.regs.$register = v; 1 }
    )
}

macro_rules! dec_byte {
    ($register:ident) => (
        |cpu: &mut CPU| { let mut v = cpu.regs.$register; cpu.dec_byte(&mut v); cpu.regs.$register = v; 1 }
    )
}

macro_rules! inc_word {
    ($register:ident) => (
        |cpu: &mut CPU| { let mut v = cpu.regs.get($register); cpu.inc_word(&mut v); cpu.regs.set($register, v); 2 }
    )
}

macro_rules! dec_word {
    ($register:ident) => (
        |cpu: &mut CPU| { let mut v = cpu.regs.get($register); cpu.dec_word(&mut v); cpu.regs.set($register, v); 2 }
    )
}


impl CPU {
    const OPS: &'static [Operation] = &[
        dop!("NOP"                , &CPU::nop), // 0x00 NOP
        dop!("LD BC,{:#4X}"  , u16, &|cpu: &mut CPU, value| { cpu.regs.set_bc(value); 3 }), // 0x01 LD BC,d16
        dop!("LD (BC),A"          , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_bc(), cpu.regs.a); 2 }), // 0x02 LD (BC),A
        dop!("INC BC"             , &inc_word!(BC)), // 0x03 INC BC
        dop!("INC B"              , &inc_byte!(b)), // 0x04 INC B
        dop!("DEC B"              , &dec_byte!(b)), // 0x05 DEC B
        dop!("LD B,{:#2X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.b = value; 2 }), // 0x06 LD B,d8
        dop!("RLCA"               , &CPU::rotate_left_circular_accumulator), // 0x07 RLCA
        dop!("LD ({:#4x}),SP", u16, &|cpu: &mut CPU, addr| { cpu.mmu.write_word(addr, cpu.regs.sp); 5 }), // 0x08 LD (a16),SP
        dop!("ADD HL,BC"          , &|cpu: &mut CPU| { let v = cpu.regs.get_bc(); cpu.add_to_hl(v); 2 }), // 0x09 ADD HL,BC
        dop!("LD A,(BC)"          , &|cpu: &mut CPU| { let a = cpu.regs.get_bc(); cpu.regs.a = cpu.mmu.read_byte(a); 2 }), // 0x0A LD A,(BC)
        dop!("DEC BC"             , &dec_word!(BC)), // 0x0B DEC BC
        dop!("INC C"              , &inc_byte!(c)), // 0x0C INC C
        dop!("DEC C"              , &dec_byte!(c)), // 0x0D DEC C
        dop!("LD C,{:#2X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.c = value; 2 }), // 0x0E LD C,d8
        dop!("RRCA"               , &CPU::rotate_right_circular_accumulator), // 0x0F RRCA

        dop!("STOP"               , &CPU::unimplemented), // 0x10 STOP
        dop!("LD DE,{:#4X}"  , u16, &|cpu: &mut CPU, value| { cpu.regs.set_de(value); 3 }), // 0x11 LD DE,d16
        dop!("LD (DE),A"          , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_de(), cpu.regs.a); 2 }), // 0x12 LD (DE),A
        dop!("INC DE"             , &inc_word!(DE)), // 0x13 INC DE
        dop!("INC D"              , &inc_byte!(d)), // 0x14 INC D
        dop!("DEC D"              , &dec_byte!(d)), // 0x15 DEC D
        dop!("LD D,{:#2X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.d = value; 2 }), // 0x16 LD D,d8
        dop!("RLA"                , &CPU::rotate_left_accumulator), // 0x07 RLA
        dop!("JR {:#2X}"     , u8 , &CPU::unimplemented_8), // 0x18 JR r8
        dop!("ADD HL,DE"          , &|cpu: &mut CPU| { let v = cpu.regs.get_de(); cpu.add_to_hl(v); 2 }), // 0x19 ADD HL,DE
        dop!("LD A,(DE)"          , &|cpu: &mut CPU| { let a = cpu.regs.get_de(); cpu.regs.a = cpu.mmu.read_byte(a); 2 }), // 0x1A LD A,(DE)
        dop!("DEC DE"             , &dec_word!(DE)), // 0x1B DEC DE
        dop!("INC E"              , &inc_byte!(e)), // 0x1C INC E
        dop!("DEC E"              , &dec_byte!(e)), // 0x1D DEC E
        dop!("LD E,{:#2X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.e = value; 2 }), // 0x1E LD E,d8
        dop!("RRA"                , &CPU::rotate_right_accumulator), // 0x1F RRA

        dop!("JR NZ,{:#2X}"  , u8 , &CPU::unimplemented_8), // 0x20 JR NZ,r8
        dop!("LD HL,{:#4X}"  , u16, &|cpu: &mut CPU, value| { cpu.regs.set_hl(value); 3 }), // 0x21 LD HL,d16
        dop!("LD (HL+),A"         , &CPU::unimplemented), // 0x22 LD (HL+),A
        dop!("INC HL"             , &inc_word!(HL)), // 0x23 INC HL
        dop!("INC H"              , &inc_byte!(h)), // 0x24 INC H
        dop!("DEC H"              , &dec_byte!(h)), // 0x25 DEC H
        dop!("LD H,{:#2X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.h = value; 2 }), // 0x26 LD H,d8
        dop!("DDA"                , &CPU::decimal_adjust_accumulator), // 0x27 DAA
        dop!("JR Z,{:#2X}"   , u8 , &CPU::unimplemented_8), // 0x28 JR Z,r8
        dop!("ADD HL,HL"          , &|cpu: &mut CPU| { let v = cpu.regs.get_hl(); cpu.add_to_hl(v); 2 }), // 0x29 ADD HL,HL
        dop!("LD A,(HL+)"         , &CPU::unimplemented), // 0x2A LD A,(HL+)
        dop!("DEC HL"             , &dec_word!(HL)), // 0x2B DEC HL
        dop!("INC L"              , &inc_byte!(l)), // 0x2C INC L
        dop!("DEC L"              , &dec_byte!(l)), // 0x2D DEC L
        dop!("LD L,{:#2X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.l = value; 2 }), // 0x2E LD L,d8
        dop!("CPL"                , &CPU::complement), // 0x2F CPL

        dop!("JR NC,{:#2X}"  , u8 , &CPU::unimplemented_8), // 0x30 JR NC,r8
        dop!("LD SP,{:#4X}"  , u16, &|cpu: &mut CPU, value| { cpu.regs.sp = value; 3 }), // 0x31 LD SP,d16
        dop!("LD (HL-),A"         , &CPU::unimplemented), // 0x32 LD (HL-),A
        dop!("INC SP"             , &inc_word!(SP)), // 0x34 INC SP
        dop!("INC (HL)"           , &|cpu: &mut CPU| { let a = cpu.regs.get_hl(); let mut v = cpu.mmu.read_byte(a); cpu.inc_byte(&mut v); cpu.mmu.write_byte(a, v); 3 }), // 0x34 INC (HL)
        dop!("DEC (HL)"           , &|cpu: &mut CPU| { let a = cpu.regs.get_hl(); let mut v = cpu.mmu.read_byte(a); cpu.dec_byte(&mut v); cpu.mmu.write_byte(a, v); 3 }), // 0x35 DEC (HL)
        dop!("LD (HL),{:#2X}", u8 , &|cpu: &mut CPU, value| { let a = cpu.regs.get_hl(); cpu.mmu.write_byte(a, value); 3 }), // 0x36 LD (HL),d8
        dop!("SCF"                , &CPU::set_carry_flag), // 0x37 SCF
        dop!("JR C,{:#2X}"   , u8 , &CPU::unimplemented_8), // 0x38 JR C,r8
        dop!("ADD HL,SP"          , &|cpu: &mut CPU| { let v = cpu.regs.sp; cpu.add_to_hl(v); 2 }), // 0x39 ADD HL,SP
        dop!("LD A,(HL-)"         , &CPU::unimplemented), // 0x3A LD A,(HL-)
        dop!("DEC SP"             , &dec_word!(SP)), // 0x3B DEC SP
        dop!("INC A"              , &inc_byte!(a)), // 0x3C INC A
        dop!("DEC A"              , &dec_byte!(a)), // 0x3D DEC A
        dop!("LD A,{:#2X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.a = value; 2 }), // 0x3E LD A,d8
        dop!("CCF"                , &CPU::complement_carry_flag), // 0x3F CCF
    ];

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
        let op_code = self.fetch_byte();
        let op = &CPU::OPS[op_code as usize];
        let op_impl = op.execute;
        op_impl(self)
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

    // Misc/Flow Control

    fn nop(&mut self) -> usize {
        1
    }

    fn unimplemented(&mut self) -> usize {
        unimplemented!("op is unimplemented")
    }

    fn unimplemented_8(&mut self, _value: u8) -> usize {
        unimplemented!("op is unimplemented")
    }

    // ALU

    fn inc_byte(&mut self, value: &mut u8) {
        let result = value.wrapping_add(1);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, (*value & 0x0F) + 1 > 0x0F);
        *value = result;
    }

    fn inc_word(&mut self, value: &mut u16) {
        *value = value.wrapping_add(1)
    }

    fn dec_byte(&mut self, value: &mut u8) {
        let result = value.wrapping_sub(1);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, true);
        self.regs.set_flag(H, (*value & 0x0F) == 0x0F);
        *value = result;
    }

    fn dec_word(&mut self, value: &mut u16) {
        *value = value.wrapping_sub(1)
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
    fn decimal_adjust_accumulator(&mut self) -> usize {
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

    fn complement(&mut self) -> usize {
        self.regs.set_flag(N, true);
        self.regs.set_flag(H, true);
        self.regs.a = !self.regs.a;
        1
    }

    fn set_carry_flag(&mut self) -> usize {
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, true);
        1
    }

    fn complement_carry_flag(&mut self) -> usize {
        let c = !self.regs.get_flag(C);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, c);
        1
    }

    // Rotates

    fn rotate_left_circular_accumulator(&mut self) -> usize {
        let a = self.regs.a;
        self.regs.set_flag(Z, false);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, (a & (1 << 7)) != 0);
        self.regs.a = a.rotate_left(1);
        1
    }

    fn rotate_left_accumulator(&mut self) -> usize {
        let a = self.regs.a;
        let c = self.regs.get_flag(C);
        self.regs.set_flag(Z, false);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, (a & (1 << 7)) != 0);
        self.regs.a = a << 1 | (if c { 1 } else { 0 });
        1
    }

    fn rotate_right_circular_accumulator(&mut self) -> usize {
        let a = self.regs.a;
        self.regs.set_flag(Z, false);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, (a & 1) > 0);
        self.regs.a = a.rotate_right(1);
        1
    }

    fn rotate_right_accumulator(&mut self) -> usize {
        let a = self.regs.a;
        let c = self.regs.get_flag(C);
        self.regs.set_flag(Z, false);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, (a & 1) != 0);
        self.regs.a = a >> 1 | (if c { 1 << 7 } else { 0 });
        1
    }

    // DEBUG

    pub fn print_reg_state(&self) {
        println!("Hello, world! {:?}", self.regs);
    }
}
