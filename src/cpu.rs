use registers::Registers;
use registers::Flag::{ Z, N, H, C };
use registers::WordRegister::{ BC, DE, HL, SP };
use mmu::MMU;
use std::{thread, time};
use utils::format::FormatAsSigned;

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
    ($name:tt, i8, $execute:expr) => (Operation {
        execute: &|cpu: &mut CPU| {
            let value = cpu.fetch_byte() as i8;
            println!($name, FormatAsSigned(value));
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
        |cpu: &mut CPU| {
            let mut v = cpu.regs.$register;
            cpu.inc_byte(&mut v);
            cpu.regs.$register = v;
            1
        }
    )
}

macro_rules! dec_byte {
    ($register:ident) => (
        |cpu: &mut CPU| {
            let mut v = cpu.regs.$register;
            cpu.dec_byte(&mut v);
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
            cpu.inc_word(&mut v);
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
            cpu.dec_word(&mut v);
            cpu.regs.set($register, v);
            2
        }
    )
}

macro_rules! ld {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            cpu.regs.$source_register = cpu.regs.$target_register;
            1
        }
    )
}

macro_rules! add {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            let addend = cpu.regs.$source_register;
            let mut v = cpu.regs.$target_register;
            v = cpu.add_byte(v, addend);
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
            v = cpu.add_byte_with_carry(v, addend);
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
            v = cpu.sub_byte(v, addend);
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
            v = cpu.sub_byte_with_carry(v, addend);
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
        dop!("JR {:#02X}"    , i8 , &CPU::relative_jump), // 0x18 JR r8
        dop!("ADD HL,DE"          , &|cpu: &mut CPU| { let v = cpu.regs.get_de(); cpu.add_to_hl(v); 2 }), // 0x19 ADD HL,DE
        dop!("LD A,(DE)"          , &|cpu: &mut CPU| { let a = cpu.regs.get_de(); cpu.regs.a = cpu.mmu.read_byte(a); 2 }), // 0x1A LD A,(DE)
        dop!("DEC DE"             , &dec_word!(DE)), // 0x1B DEC DE
        dop!("INC E"              , &inc_byte!(e)), // 0x1C INC E
        dop!("DEC E"              , &dec_byte!(e)), // 0x1D DEC E
        dop!("LD E,{:#2X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.e = value; 2 }), // 0x1E LD E,d8
        dop!("RRA"                , &CPU::rotate_right_accumulator), // 0x1F RRA

        dop!("JR NZ,{:#02X}"  , i8 , &CPU::relative_jump_nz), // 0x20 JR NZ,r8
        dop!("LD HL,{:#4X}"  , u16, &|cpu: &mut CPU, value| { cpu.regs.set_hl(value); 3 }), // 0x21 LD HL,d16
        dop!("LD (HL+),A"         , &CPU::unimplemented), // 0x22 LD (HL+),A
        dop!("INC HL"             , &inc_word!(HL)), // 0x23 INC HL
        dop!("INC H"              , &inc_byte!(h)), // 0x24 INC H
        dop!("DEC H"              , &dec_byte!(h)), // 0x25 DEC H
        dop!("LD H,{:#2X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.h = value; 2 }), // 0x26 LD H,d8
        dop!("DDA"                , &CPU::decimal_adjust_accumulator), // 0x27 DAA
        dop!("JR Z,{:#2X}"   , i8 , &CPU::relative_jump_z), // 0x28 JR Z,r8
        dop!("ADD HL,HL"          , &|cpu: &mut CPU| { let v = cpu.regs.get_hl(); cpu.add_to_hl(v); 2 }), // 0x29 ADD HL,HL
        dop!("LD A,(HL+)"         , &CPU::unimplemented), // 0x2A LD A,(HL+)
        dop!("DEC HL"             , &dec_word!(HL)), // 0x2B DEC HL
        dop!("INC L"              , &inc_byte!(l)), // 0x2C INC L
        dop!("DEC L"              , &dec_byte!(l)), // 0x2D DEC L
        dop!("LD L,{:#2X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.l = value; 2 }), // 0x2E LD L,d8
        dop!("CPL"                , &CPU::complement), // 0x2F CPL

        dop!("JR NC,{:#2X}"  , i8 , &CPU::relative_jump_nc), // 0x30 JR NC,r8
        dop!("LD SP,{:#4X}"  , u16, &|cpu: &mut CPU, value| { cpu.regs.sp = value; 3 }), // 0x31 LD SP,d16
        dop!("LD (HL-),A"         , &CPU::unimplemented), // 0x32 LD (HL-),A
        dop!("INC SP"             , &inc_word!(SP)), // 0x34 INC SP
        dop!("INC (HL)"           , &|cpu: &mut CPU| { let a = cpu.regs.get_hl(); let mut v = cpu.mmu.read_byte(a); cpu.inc_byte(&mut v); cpu.mmu.write_byte(a, v); 3 }), // 0x34 INC (HL)
        dop!("DEC (HL)"           , &|cpu: &mut CPU| { let a = cpu.regs.get_hl(); let mut v = cpu.mmu.read_byte(a); cpu.dec_byte(&mut v); cpu.mmu.write_byte(a, v); 3 }), // 0x35 DEC (HL)
        dop!("LD (HL),{:#2X}", u8 , &|cpu: &mut CPU, value| { let a = cpu.regs.get_hl(); cpu.mmu.write_byte(a, value); 3 }), // 0x36 LD (HL),d8
        dop!("SCF"                , &CPU::set_carry_flag), // 0x37 SCF
        dop!("JR C,{:#2X}"   , i8 , &CPU::relative_jump_c), // 0x38 JR C,r8
        dop!("ADD HL,SP"          , &|cpu: &mut CPU| { let v = cpu.regs.sp; cpu.add_to_hl(v); 2 }), // 0x39 ADD HL,SP
        dop!("LD A,(HL-)"         , &CPU::unimplemented), // 0x3A LD A,(HL-)
        dop!("DEC SP"             , &dec_word!(SP)), // 0x3B DEC SP
        dop!("INC A"              , &inc_byte!(a)), // 0x3C INC A
        dop!("DEC A"              , &dec_byte!(a)), // 0x3D DEC A
        dop!("LD A,{:#2X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.a = value; 2 }), // 0x3E LD A,d8
        dop!("CCF"                , &CPU::complement_carry_flag), // 0x3F CCF

        dop!("LD B,B"             , &ld!(b,b)), // 0x40 LD B,B
        dop!("LD B,C"             , &ld!(b,c)), // 0x41 LD B,C
        dop!("LD B,D"             , &ld!(b,d)), // 0x42 LD B,D
        dop!("LD B,E"             , &ld!(b,e)), // 0x43 LD B,E
        dop!("LD B,H"             , &ld!(b,h)), // 0x44 LD B,H
        dop!("LD B,L"             , &ld!(b,l)), // 0x45 LD B,L
        dop!("LD B,(HL)"          , &|cpu: &mut CPU| { cpu.regs.b = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x46 LD B,(HL)
        dop!("LD B,A"             , &ld!(b,a)), // 0x47 LD B,A
        dop!("LD C,B"             , &ld!(c,b)), // 0x48 LD C,B
        dop!("LD C,C"             , &ld!(c,c)), // 0x49 LD C,C
        dop!("LD C,D"             , &ld!(c,d)), // 0x4A LD C,D
        dop!("LD C,E"             , &ld!(c,e)), // 0x4B LD C,E
        dop!("LD C,H"             , &ld!(c,h)), // 0x4C LD C,H
        dop!("LD C,L"             , &ld!(c,l)), // 0x4D LD C,L
        dop!("LD C,(HL)"          , &|cpu: &mut CPU| { cpu.regs.c = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x4E LD C,(HL)
        dop!("LD C,A"             , &ld!(b,a)), // 0x4F LD C,A

        dop!("LD D,B"             , &ld!(d,b)), // 0x50 LD B,B
        dop!("LD D,C"             , &ld!(d,c)), // 0x51 LD B,C
        dop!("LD D,D"             , &ld!(d,d)), // 0x52 LD B,D
        dop!("LD D,E"             , &ld!(d,e)), // 0x53 LD B,E
        dop!("LD D,H"             , &ld!(d,h)), // 0x54 LD B,H
        dop!("LD D,L"             , &ld!(d,l)), // 0x55 LD B,L
        dop!("LD D,(HL)"          , &|cpu: &mut CPU| { cpu.regs.d = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x56 LD D,(HL)
        dop!("LD D,A"             , &ld!(d,a)), // 0x57 LD B,A
        dop!("LD E,B"             , &ld!(e,b)), // 0x58 LD C,B
        dop!("LD E,C"             , &ld!(e,c)), // 0x59 LD C,C
        dop!("LD E,D"             , &ld!(e,d)), // 0x5A LD C,D
        dop!("LD E,E"             , &ld!(e,e)), // 0x5B LD C,E
        dop!("LD E,H"             , &ld!(e,h)), // 0x5C LD C,H
        dop!("LD E,L"             , &ld!(e,l)), // 0x5D LD C,L
        dop!("LD E,(HL)"          , &|cpu: &mut CPU| { cpu.regs.e = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x5E LD E,(HL)
        dop!("LD E,A"             , &ld!(e,a)), // 0x5F LD C,A

        dop!("LD H,B"             , &ld!(h,b)), // 0x60 LD H,B
        dop!("LD H,C"             , &ld!(h,c)), // 0x61 LD H,C
        dop!("LD H,D"             , &ld!(h,d)), // 0x62 LD H,D
        dop!("LD H,E"             , &ld!(h,e)), // 0x63 LD H,E
        dop!("LD H,H"             , &ld!(h,h)), // 0x64 LD H,H
        dop!("LD H,L"             , &ld!(h,l)), // 0x65 LD H,L
        dop!("LD H,(HL)"          , &|cpu: &mut CPU| { cpu.regs.h = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x66 LD H,(HL)
        dop!("LD H,A"             , &ld!(h,a)), // 0x67 LD H,A
        dop!("LD L,B"             , &ld!(l,b)), // 0x68 LD L,B
        dop!("LD L,C"             , &ld!(l,c)), // 0x69 LD L,C
        dop!("LD L,D"             , &ld!(l,d)), // 0x6A LD L,D
        dop!("LD L,E"             , &ld!(l,e)), // 0x6B LD L,E
        dop!("LD L,H"             , &ld!(l,h)), // 0x6C LD L,H
        dop!("LD L,L"             , &ld!(l,l)), // 0x6D LD L,L
        dop!("LD L,(HL)"          , &|cpu: &mut CPU| { cpu.regs.l = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x6E LD L,(HL)
        dop!("LD L,A"             , &ld!(l,a)), // 0x6F LD L,A

        dop!("LD (HL),B"          , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.b); 2 }), // 0x70 LD (HL),B
        dop!("LD (HL),C"          , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.c); 2 }), // 0x71 LD (HL),C
        dop!("LD (HL),D"          , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.d); 2 }), // 0x72 LD (HL),D
        dop!("LD (HL),E"          , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.e); 2 }), // 0x73 LD (HL),E
        dop!("LD (HL),H"          , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.h); 2 }), // 0x74 LD (HL),H
        dop!("LD (HL),L"          , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.l); 2 }), // 0x75 LD (HL),L
        dop!("HALT"               , &CPU::unimplemented), // 0x76 HALT
        dop!("LD (HL),L"          , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.a); 2 }), // 0x77 LD (HL),L
        dop!("LD A,B"             , &ld!(a,b)), // 0x78 LD A,B
        dop!("LD A,C"             , &ld!(a,c)), // 0x79 LD A,C
        dop!("LD A,D"             , &ld!(a,d)), // 0x7A LD A,D
        dop!("LD A,E"             , &ld!(a,e)), // 0x7B LD A,E
        dop!("LD A,H"             , &ld!(a,h)), // 0x7C LD A,H
        dop!("LD A,L"             , &ld!(a,l)), // 0x7D LD A,L
        dop!("LD A,(HL)"          , &|cpu: &mut CPU| { cpu.regs.a = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x7E LD A,(HL)
        dop!("LD A,A"             , &ld!(a,a)), // 0x7F LD A,A

        dop!("ADD A,B"            , &add!(a,b)),
        dop!("ADD A,C"            , &add!(a,c)),
        dop!("ADD A,D"            , &add!(a,d)),
        dop!("ADD A,E"            , &add!(a,e)),
        dop!("ADD A,H"            , &add!(a,h)),
        dop!("ADD A,L"            , &add!(a,l)),
        dop!("ADD A,(HL)"         , &CPU::unimplemented),
        dop!("ADD A,A"            , &add!(a,a)),
        dop!("ADC A,B"            , &adc!(a,b)),
        dop!("ADC A,C"            , &adc!(a,c)),
        dop!("ADC A,D"            , &adc!(a,d)),
        dop!("ADC A,E"            , &adc!(a,e)),
        dop!("ADC A,H"            , &adc!(a,h)),
        dop!("ADC A,L"            , &adc!(a,l)),
        dop!("ADC A,(HL)"         , &CPU::unimplemented),
        dop!("ADC A,A"            , &adc!(a,a)),

        dop!("SUB A,B"            , &sub!(a,b)),
        dop!("SUB A,C"            , &sub!(a,c)),
        dop!("SUB A,D"            , &sub!(a,d)),
        dop!("SUB A,E"            , &sub!(a,e)),
        dop!("SUB A,H"            , &sub!(a,h)),
        dop!("SUB A,L"            , &sub!(a,l)),
        dop!("SUB A,(HL)"         , &CPU::unimplemented),
        dop!("SUB A,A"            , &sub!(a,a)),
        dop!("SBC A,B"            , &sbc!(a,b)),
        dop!("SBC A,C"            , &sbc!(a,c)),
        dop!("SBC A,D"            , &sbc!(a,d)),
        dop!("SBC A,E"            , &sbc!(a,e)),
        dop!("SBC A,H"            , &sbc!(a,h)),
        dop!("SBC A,L"            , &sbc!(a,l)),
        dop!("SBC A,(HL)"         , &CPU::unimplemented),
        dop!("SBC A,A"            , &sbc!(a,a)),

        dop!("AND A,B"            , &and!(a,b)),
        dop!("AND A,C"            , &and!(a,c)),
        dop!("AND A,D"            , &and!(a,d)),
        dop!("AND A,E"            , &and!(a,e)),
        dop!("AND A,H"            , &and!(a,h)),
        dop!("AND A,L"            , &and!(a,l)),
        dop!("AND A,(HL)"         , &CPU::unimplemented),
        dop!("AND A,A"            , &xor!(a,a)),
        dop!("XOR A,B"            , &xor!(a,b)),
        dop!("XOR A,C"            , &xor!(a,c)),
        dop!("XOR A,D"            , &xor!(a,d)),
        dop!("XOR A,E"            , &xor!(a,e)),
        dop!("XOR A,H"            , &xor!(a,h)),
        dop!("XOR A,L"            , &xor!(a,l)),
        dop!("XOR A,(HL)"         , &CPU::unimplemented),
        dop!("XOR A,A"            , &xor!(a,a)),

        dop!("OR A,B"             , &or!(a,b)),
        dop!("OR A,C"             , &or!(a,c)),
        dop!("OR A,D"             , &or!(a,d)),
        dop!("OR A,E"             , &or!(a,e)),
        dop!("OR A,H"             , &or!(a,h)),
        dop!("OR A,L"             , &or!(a,l)),
        dop!("OR A,(HL)"          , &CPU::unimplemented),
        dop!("OR A,A"             , &or!(a,a)),
        dop!("CP A,B"             , &cp!(a,b)),
        dop!("CP A,C"             , &cp!(a,c)),
        dop!("CP A,D"             , &cp!(a,d)),
        dop!("CP A,E"             , &cp!(a,e)),
        dop!("CP A,H"             , &cp!(a,h)),
        dop!("CP A,L"             , &cp!(a,l)),
        dop!("CP A,(HL)"          , &CPU::unimplemented),
        dop!("CP A,A"             , &cp!(a,a)),

        dop!("RET NZ"             , &CPU::stack_ret_nz),
        dop!("POP BC"             , &CPU::unimplemented),
        dop!("JP NZ,{:#4X}"  , u16, &CPU::jump_nz),
        dop!("JP {:#4X}"     , u16, &CPU::jump),
        dop!("CALL NZ,{:#4X}", u16, &CPU::stack_call_nz),
        dop!("PUSH BC"            , &CPU::stack_push_bc),
        dop!("ADD A,{:#2X}"  , u8 , &CPU::unimplemented_8),
        dop!("RST 00H"            , &CPU::unimplemented),
        dop!("RET Z"              , &CPU::stack_ret_z),
        dop!("RET"                , &CPU::stack_ret),
        dop!("JP Z,{:#4X}"   , u16, &CPU::jump_z),
        dop!("PREFIX CB"          , &CPU::unimplemented),
        dop!("CALL Z,{:#4X}" , u16, &CPU::stack_call_z),
        dop!("CALL {:#4X}"   , u16, &CPU::stack_call),
        dop!("ADC A,{:#2X}"  , u8 , &CPU::unimplemented_8),
        dop!("RST 08H"            , &CPU::unimplemented),

        dop!("RET NC"             , &CPU::stack_ret_nc),
        dop!("POP DE"             , &CPU::unimplemented),
        dop!("JP NC,{:#4X}"  , u16, &CPU::jump_nc),
        dop!("0xD3 NOPE"          , &CPU::nonexistant),
        dop!("CALL NC,{:#4X}", u16, &CPU::stack_call_nc),
        dop!("PUSH DE"            , &CPU::stack_push_de),
        dop!("SUB A,{:#2X}"  , u8 , &CPU::unimplemented_8),
        dop!("RST 10H"            , &CPU::unimplemented),
        dop!("RET C"              , &CPU::stack_ret_c),
        dop!("RETI"               , &CPU::unimplemented),
        dop!("JP C,{:#4X}"   , u16, &CPU::jump_c),
        dop!("0xDB NOPE"          , &CPU::nonexistant),
        dop!("CALL C,{:#4X}" , u16, &CPU::stack_call_c),
        dop!("0xDD NOPE"          , &CPU::nonexistant),
        dop!("SBC A,{:#2X}"  , u8 , &CPU::unimplemented_8),
        dop!("RST 18H"            , &CPU::unimplemented),

        dop!("LDH ({:#2X}),A", u8 , &|cpu: &mut CPU, addr| { let value = cpu.regs.a; cpu.mmu.write_byte(addr as u16 + 0xFF00, value); 3 }),
        dop!("POP HL"             , &CPU::unimplemented),
        dop!("LD (C),A"           , &|cpu: &mut CPU| { let addr = cpu.regs.c; let value = cpu.regs.a; cpu.mmu.write_byte(addr as u16 + 0xFF00, value); 2 }),
        dop!("0xE3 NOPE"          , &CPU::nonexistant),
        dop!("0xE4 NOPE"          , &CPU::nonexistant),
        dop!("PUSH HL"            , &CPU::stack_push_hl),
        dop!("AND A,{:#2X}"  , u8 , &CPU::unimplemented_8),
        dop!("RST 20H"            , &CPU::unimplemented),
        dop!("ADD SP,{:#2X}" , u8 , &CPU::unimplemented_8),
        dop!("JP (HL)"            , &CPU::unimplemented),
        dop!("LD ({:#4X}),A" , u16, &|cpu: &mut CPU, addr| { cpu.mmu.write_byte(addr, cpu.regs.a); 4 }),
        dop!("0xEB NOPE"          , &CPU::nonexistant),
        dop!("0xEC NOPE"          , &CPU::nonexistant),
        dop!("0xED NOPE"          , &CPU::nonexistant),
        dop!("XOR {:#2X}"    , u8 , &CPU::unimplemented_8),
        dop!("RST 28H"            , &CPU::unimplemented),

        dop!("LDH A,({:#2X})", u8 , &|cpu: &mut CPU, addr| { cpu.regs.a = cpu.mmu.read_byte(addr as u16 + 0xFF00); 3 }),
        dop!("POP AF"             , &|cpu: &mut CPU| { let value = cpu.stack_pop(); cpu.regs.set_af(value); 3 }),
        dop!("LD A,(C)"           , &|cpu: &mut CPU| { let addr = cpu.regs.c; cpu.regs.a = cpu.mmu.read_byte(addr as u16 + 0xFF00); 2 }),
        dop!("DI"                 , &CPU::enable_interupts),
        dop!("0xF4 NOPE"          , &CPU::nonexistant),
        dop!("PUSH AF"            , &CPU::stack_push_af),
        dop!("OR {:#2X}"     , u8 , &CPU::unimplemented_8),
        dop!("RST 30H"            , &CPU::unimplemented),
        dop!("LD HL,SP+{:#2X}", u8, &CPU::unimplemented_8),
        dop!("LD SP,HL"           , &CPU::unimplemented),
        dop!("LD A,({:#4X})" , u16, &CPU::unimplemented_16),
        dop!("DI"                 , &CPU::disable_interupts),
        dop!("0xFC NOPE"          , &CPU::nonexistant),
        dop!("0xFD NOPE"          , &CPU::nonexistant),
        dop!("CP {:#2X}"     , u8 , &CPU::unimplemented_8),
        dop!("RST 38H"            , &CPU::unimplemented),

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

        let time = time::Duration::from_millis(100);
        thread::sleep(time);

        let ticks = op_impl(self);

        println!("--> {:#4X}", self.regs.pc);

        return ticks;
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

    fn enable_interupts(&mut self) -> usize {
        println!("INTERUPTS NOT IMPLEMENTED YET");
        1
    }

    fn disable_interupts(&mut self) -> usize {
        println!("INTERUPTS NOT IMPLEMENTED YET");
        1
    }

    fn nonexistant(&mut self) -> usize {
        unimplemented!("this op doesn't exist on gb hardware")
    }

    fn unimplemented(&mut self) -> usize {
        unimplemented!("op is unimplemented")
    }

    fn unimplemented_8(&mut self, _value: u8) -> usize {
        unimplemented!("op is unimplemented")
    }

    fn unimplemented_16(&mut self, _value: u16) -> usize {
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
        self.regs.set_flag(H, ((lhs & 0x07FF) + (rhs & 0x07FF)) > 0x07FF);
        self.regs.set_flag(C, lhs > 0xFFFF - rhs);
        result
    }

    fn add_byte(&mut self, lhs: u8, rhs: u8) -> u8 {
        let result = lhs.wrapping_add(rhs);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, ((lhs & 0x0F) + (rhs & 0x0F)) > 0x0F);
        self.regs.set_flag(C, lhs > 0xFF - rhs);
        result
    }

    fn add_byte_with_carry(&mut self, lhs: u8, rhs: u8) -> u8 {
        let carry = if self.regs.get_flag(C) { 1 } else { 0 };
        let result = lhs.wrapping_add(rhs).wrapping_add(carry);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, ((lhs & 0x0F) + (rhs & 0x0F)).wrapping_add(carry) > 0x0F);
        self.regs.set_flag(C, lhs > 0xFF - rhs - carry);
        result
    }

    fn sub_byte(&mut self, lhs: u8, rhs: u8) -> u8 {
        let result = lhs.wrapping_sub(rhs);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, true);
        self.regs.set_flag(H, (lhs & 0x0F) < (rhs & 0x0F));
        self.regs.set_flag(C, lhs < rhs);
        result
    }

    fn sub_byte_with_carry(&mut self, lhs: u8, rhs: u8) -> u8 {
        let carry = if self.regs.get_flag(C) { 1 } else { 0 };
        let result = lhs.wrapping_add(rhs).wrapping_add(carry);
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, true);
        self.regs.set_flag(H, ((lhs & 0x0F) < (rhs & 0x0F).wrapping_add(carry)));
        self.regs.set_flag(C, (lhs as u16) < (rhs as u16) + (carry as u16));
        result
    }

    fn alu_and(&mut self, lhs: u8, rhs: u8) -> u8 {
        let result = lhs & rhs;
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, true);
        self.regs.set_flag(C, false);
        result
    }

    fn alu_xor(&mut self, lhs: u8, rhs: u8) -> u8 {
        let result = lhs ^ rhs;
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, false);
        result
    }

    fn alu_or(&mut self, lhs: u8, rhs: u8) -> u8 {
        let result = lhs ^ rhs;
        self.regs.set_flag(Z, result == 0);
        self.regs.set_flag(N, false);
        self.regs.set_flag(H, false);
        self.regs.set_flag(C, false);
        result
    }

    fn alu_cp(&mut self, lhs: u8, rhs: u8) {
        self.sub_byte(lhs, rhs);
    }

    // TODO: using match here is unnecessary... can be done more concisely by mutating through conditions
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

    // Jumps

    fn jump_nz(&mut self, addr:u16) -> usize {
        if !self.regs.get_flag(Z) {
            return self.jump(addr)
        }
        2
    }

    fn jump_nc(&mut self, addr:u16) -> usize {
        if !self.regs.get_flag(C) {
            return self.jump(addr)
        }
        2
    }

    fn jump_z(&mut self, addr:u16) -> usize {
        if self.regs.get_flag(Z) {
            return self.jump(addr)
        }
        2
    }

    fn jump_c(&mut self, addr:u16) -> usize {
        if self.regs.get_flag(C) {
            return self.jump(addr)
        }
        2
    }

    fn jump(&mut self, addr:u16) -> usize {
        self.regs.pc = addr;
        4
    }

    fn relative_jump_nz(&mut self, addr:i8) -> usize {
        if !self.regs.get_flag(Z) {
            return self.relative_jump(addr)
        }
        2
    }

    fn relative_jump_nc(&mut self, addr:i8) -> usize {
        if !self.regs.get_flag(C) {
            return self.relative_jump(addr)
        }
        2
    }

    fn relative_jump_z(&mut self, addr:i8) -> usize {
        if self.regs.get_flag(Z) {
            return self.relative_jump(addr)
        }
        2
    }

    fn relative_jump_c(&mut self, addr:i8) -> usize {
        if self.regs.get_flag(C) {
            return self.relative_jump(addr)
        }
        2
    }

    fn relative_jump(&mut self, addr:i8) -> usize {
        self.regs.pc = (self.regs.pc as i16 + addr as i16) as u16;
        3
    }

    // Stack

    fn stack_ret_nz(&mut self) -> usize {
        if !self.regs.get_flag(Z) {
            self.regs.pc = self.stack_pop();
            return 5
        }
        2
    }

    fn stack_ret_nc(&mut self) -> usize {
        if !self.regs.get_flag(C) {
            self.regs.pc = self.stack_pop();
            return 5
        }
        2
    }

    fn stack_ret_z(&mut self) -> usize {
        if self.regs.get_flag(Z) {
            self.regs.pc = self.stack_pop();
            return 5
        }
        2
    }

    fn stack_ret_c(&mut self) -> usize {
        if self.regs.get_flag(C) {
            self.regs.pc = self.stack_pop();
            return 5
        }
        2
    }

    fn stack_ret(&mut self) -> usize {
        self.regs.pc = self.stack_pop();
        return 4
    }

    fn stack_call_nz(&mut self, value:u16) -> usize {
        if !self.regs.get_flag(Z) {
            return self.stack_call(value)
        }
        3
    }

    fn stack_call_nc(&mut self, value:u16) -> usize {
        if !self.regs.get_flag(C) {
            return self.stack_call(value)
        }
        3
    }

    fn stack_call_z(&mut self, value:u16) -> usize {
        if self.regs.get_flag(Z) {
            return self.stack_call(value)
        }
        3
    }

    fn stack_call_c(&mut self, value:u16) -> usize {
        if self.regs.get_flag(C) {
            return self.stack_call(value)
        }
        3
    }

    fn stack_call(&mut self, value:u16) -> usize {
        let next_op = self.regs.pc + 2;
        self.stack_push(next_op);
        self.regs.pc = value;
        return 6
    }

    fn stack_pop(&mut self) -> u16 {
        let result = self.mmu.read_word(self.regs.sp);
        self.regs.sp += 2;
        result
    }

    fn stack_push_af(&mut self) -> usize {
        let value = self.regs.get_af();
        self.stack_push(value);
        4
    }

    fn stack_push_bc(&mut self) -> usize {
        let value = self.regs.get_bc();
        self.stack_push(value);
        4
    }

    fn stack_push_de(&mut self) -> usize {
        let value = self.regs.get_de();
        self.stack_push(value);
        4
    }

    fn stack_push_hl(&mut self) -> usize {
        let value = self.regs.get_hl();
        self.stack_push(value);
        4
    }

    fn stack_push(&mut self, value:u16) {
        self.regs.sp -= 2;
        self.mmu.write_word(self.regs.sp, value);
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
