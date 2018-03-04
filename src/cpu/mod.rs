use registers::Registers;
use registers::WordRegister::{ BC, DE, HL, SP };
use mmu::MMU;
use utils::format::FormatAsSigned;

#[macro_use]
mod alu;
mod stack;
mod jump;
mod shift;

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

macro_rules! ld {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            cpu.regs.$source_register = cpu.regs.$target_register;
            1
        }
    )
}

impl CPU {
    const OPS: &'static [Operation] = &[
        dop!("NOP"                 , &CPU::nop), // 0x00 NOP
        dop!("LD BC,{:#06X}"  , u16, &|cpu: &mut CPU, value| { cpu.regs.set_bc(value); 3 }), // 0x01 LD BC,d16
        dop!("LD (BC),A"           , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_bc(), cpu.regs.a); 2 }), // 0x02 LD (BC),A
        dop!("INC BC"              , &alu_inc_word!(BC)), // 0x03 INC BC
        dop!("INC B"               , &alu_inc_byte!(b)), // 0x04 INC B
        dop!("DEC B"               , &alu_dec_byte!(b)), // 0x05 DEC B
        dop!("LD B,{:#04X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.b = value; 2 }), // 0x06 LD B,d8
        dop!("RLCA"                , &CPU::rotate_left_circular_accumulator), // 0x07 RLCA
        dop!("LD ({:#06X}),SP", u16, &|cpu: &mut CPU, addr| { cpu.mmu.write_word(addr, cpu.regs.sp); 5 }), // 0x08 LD (a16),SP
        dop!("ADD HL,BC"           , &|cpu: &mut CPU| { let v = cpu.regs.get_bc(); cpu.alu_add_to_hl(v); 2 }), // 0x09 ADD HL,BC
        dop!("LD A,(BC)"           , &|cpu: &mut CPU| { let a = cpu.regs.get_bc(); cpu.regs.a = cpu.mmu.read_byte(a); 2 }), // 0x0A LD A,(BC)
        dop!("DEC BC"              , &alu_dec_word!(BC)), // 0x0B DEC BC
        dop!("INC C"               , &alu_inc_byte!(c)), // 0x0C INC C
        dop!("DEC C"               , &alu_dec_byte!(c)), // 0x0D DEC C
        dop!("LD C,{:#04X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.c = value; 2 }), // 0x0E LD C,d8
        dop!("RRCA"                , &CPU::rotate_right_circular_accumulator), // 0x0F RRCA

        dop!("STOP"                , &CPU::stop), // 0x10 STOP
        dop!("LD DE,{:#06X}"  , u16, &|cpu: &mut CPU, value| { cpu.regs.set_de(value); 3 }), // 0x11 LD DE,d16
        dop!("LD (DE),A"           , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_de(), cpu.regs.a); 2 }), // 0x12 LD (DE),A
        dop!("INC DE"              , &alu_inc_word!(DE)), // 0x13 INC DE
        dop!("INC D"               , &alu_inc_byte!(d)), // 0x14 INC D
        dop!("DEC D"               , &alu_dec_byte!(d)), // 0x15 DEC D
        dop!("LD D,{:#04X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.d = value; 2 }), // 0x16 LD D,d8
        dop!("RLA"                 , &CPU::rotate_left_accumulator), // 0x07 RLA
        dop!("JR {:#02X}"     , i8 , &CPU::relative_jump), // 0x18 JR r8
        dop!("ADD HL,DE"           , &|cpu: &mut CPU| { let v = cpu.regs.get_de(); cpu.alu_add_to_hl(v); 2 }), // 0x19 ADD HL,DE
        dop!("LD A,(DE)"           , &|cpu: &mut CPU| { let a = cpu.regs.get_de(); cpu.regs.a = cpu.mmu.read_byte(a); 2 }), // 0x1A LD A,(DE)
        dop!("DEC DE"              , &alu_dec_word!(DE)), // 0x1B DEC DE
        dop!("INC E"               , &alu_inc_byte!(e)), // 0x1C INC E
        dop!("DEC E"               , &alu_dec_byte!(e)), // 0x1D DEC E
        dop!("LD E,{:#04X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.e = value; 2 }), // 0x1E LD E,d8
        dop!("RRA"                 , &CPU::rotate_right_accumulator), // 0x1F RRA

        dop!("JR NZ,{:#02X}"  , i8 , &CPU::relative_jump_nz), // 0x20 JR NZ,r8
        dop!("LD HL,{:#06X}"  , u16, &|cpu: &mut CPU, value| { cpu.regs.set_hl(value); 3 }), // 0x21 LD HL,d16
        dop!("LD (HL+),A"          , &|cpu: &mut CPU| { let addr = cpu.regs.get_hl() + 1; cpu.mmu.write_byte(addr, cpu.regs.a); cpu.regs.set_hl(addr); 2 }), // 0x22 LD (HL+),A
        dop!("INC HL"              , &alu_inc_word!(HL)), // 0x23 INC HL
        dop!("INC H"               , &alu_inc_byte!(h)), // 0x24 INC H
        dop!("DEC H"               , &alu_dec_byte!(h)), // 0x25 DEC H
        dop!("LD H,{:#04X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.h = value; 2 }), // 0x26 LD H,d8
        dop!("DDA"                 , &CPU::alu_decimal_adjust_accumulator), // 0x27 DAA
        dop!("JR Z,{:#04X}"   , i8 , &CPU::relative_jump_z), // 0x28 JR Z,r8
        dop!("ADD HL,HL"           , &|cpu: &mut CPU| { let v = cpu.regs.get_hl(); cpu.alu_add_to_hl(v); 2 }), // 0x29 ADD HL,HL
        dop!("LD A,(HL+)"          , &|cpu: &mut CPU| { let addr = cpu.regs.get_hl() + 1; cpu.regs.a = cpu.mmu.read_byte(addr); cpu.regs.set_hl(addr); 2 }), // 0x2A LD A,(HL+)
        dop!("DEC HL"              , &alu_dec_word!(HL)), // 0x2B DEC HL
        dop!("INC L"               , &alu_inc_byte!(l)), // 0x2C INC L
        dop!("DEC L"               , &alu_dec_byte!(l)), // 0x2D DEC L
        dop!("LD L,{:#04X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.l = value; 2 }), // 0x2E LD L,d8
        dop!("CPL"                 , &CPU::alu_complement), // 0x2F CPL

        dop!("JR NC,{:#04X}"  , i8 , &CPU::relative_jump_nc), // 0x30 JR NC,r8
        dop!("LD SP,{:#06X}"  , u16, &|cpu: &mut CPU, value| { cpu.regs.sp = value; 3 }), // 0x31 LD SP,d16
        dop!("LD (HL-),A"          , &|cpu: &mut CPU| { let addr = cpu.regs.get_hl() + 2; cpu.mmu.write_byte(addr, cpu.regs.a); cpu.regs.set_hl(addr); 2 }), // 0x32 LD (HL-),A
        dop!("INC SP"              , &alu_inc_word!(SP)), // 0x34 INC SP
        dop!("INC (HL)"            , &|cpu: &mut CPU| { let a = cpu.regs.get_hl(); let mut v = cpu.mmu.read_byte(a); cpu.alu_inc_byte(&mut v); cpu.mmu.write_byte(a, v); 3 }), // 0x34 INC (HL)
        dop!("DEC (HL)"            , &|cpu: &mut CPU| { let a = cpu.regs.get_hl(); let mut v = cpu.mmu.read_byte(a); cpu.alu_dec_byte(&mut v); cpu.mmu.write_byte(a, v); 3 }), // 0x35 DEC (HL)
        dop!("LD (HL),{:#04X}", u8 , &|cpu: &mut CPU, value| { let a = cpu.regs.get_hl(); cpu.mmu.write_byte(a, value); 3 }), // 0x36 LD (HL),d8
        dop!("SCF"                 , &CPU::alu_set_carry_flag), // 0x37 SCF
        dop!("JR C,{:#04X}"   , i8 , &CPU::relative_jump_c), // 0x38 JR C,r8
        dop!("ADD HL,SP"           , &|cpu: &mut CPU| { let v = cpu.regs.sp; cpu.alu_add_to_hl(v); 2 }), // 0x39 ADD HL,SP
        dop!("LD A,(HL-)"          , &CPU::unimplemented), // 0x3A LD A,(HL-)
        dop!("DEC SP"              , &alu_dec_word!(SP)), // 0x3B DEC SP
        dop!("INC A"               , &alu_inc_byte!(a)), // 0x3C INC A
        dop!("DEC A"               , &alu_dec_byte!(a)), // 0x3D DEC A
        dop!("LD A,{:#04X}"   , u8 , &|cpu: &mut CPU, value| { cpu.regs.a = value; 2 }), // 0x3E LD A,d8
        dop!("CCF"                 , &CPU::alu_complement_carry_flag), // 0x3F CCF

        dop!("LD B,B"              , &ld!(b,b)), // 0x40 LD B,B
        dop!("LD B,C"              , &ld!(b,c)), // 0x41 LD B,C
        dop!("LD B,D"              , &ld!(b,d)), // 0x42 LD B,D
        dop!("LD B,E"              , &ld!(b,e)), // 0x43 LD B,E
        dop!("LD B,H"              , &ld!(b,h)), // 0x44 LD B,H
        dop!("LD B,L"              , &ld!(b,l)), // 0x45 LD B,L
        dop!("LD B,(HL)"           , &|cpu: &mut CPU| { cpu.regs.b = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x46 LD B,(HL)
        dop!("LD B,A"              , &ld!(b,a)), // 0x47 LD B,A
        dop!("LD C,B"              , &ld!(c,b)), // 0x48 LD C,B
        dop!("LD C,C"              , &ld!(c,c)), // 0x49 LD C,C
        dop!("LD C,D"              , &ld!(c,d)), // 0x4A LD C,D
        dop!("LD C,E"              , &ld!(c,e)), // 0x4B LD C,E
        dop!("LD C,H"              , &ld!(c,h)), // 0x4C LD C,H
        dop!("LD C,L"              , &ld!(c,l)), // 0x4D LD C,L
        dop!("LD C,(HL)"           , &|cpu: &mut CPU| { cpu.regs.c = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x4E LD C,(HL)
        dop!("LD C,A"              , &ld!(b,a)), // 0x4F LD C,A

        dop!("LD D,B"              , &ld!(d,b)), // 0x50 LD B,B
        dop!("LD D,C"              , &ld!(d,c)), // 0x51 LD B,C
        dop!("LD D,D"              , &ld!(d,d)), // 0x52 LD B,D
        dop!("LD D,E"              , &ld!(d,e)), // 0x53 LD B,E
        dop!("LD D,H"              , &ld!(d,h)), // 0x54 LD B,H
        dop!("LD D,L"              , &ld!(d,l)), // 0x55 LD B,L
        dop!("LD D,(HL)"           , &|cpu: &mut CPU| { cpu.regs.d = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x56 LD D,(HL)
        dop!("LD D,A"              , &ld!(d,a)), // 0x57 LD B,A
        dop!("LD E,B"              , &ld!(e,b)), // 0x58 LD C,B
        dop!("LD E,C"              , &ld!(e,c)), // 0x59 LD C,C
        dop!("LD E,D"              , &ld!(e,d)), // 0x5A LD C,D
        dop!("LD E,E"              , &ld!(e,e)), // 0x5B LD C,E
        dop!("LD E,H"              , &ld!(e,h)), // 0x5C LD C,H
        dop!("LD E,L"              , &ld!(e,l)), // 0x5D LD C,L
        dop!("LD E,(HL)"           , &|cpu: &mut CPU| { cpu.regs.e = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x5E LD E,(HL)
        dop!("LD E,A"              , &ld!(e,a)), // 0x5F LD C,A

        dop!("LD H,B"              , &ld!(h,b)), // 0x60 LD H,B
        dop!("LD H,C"              , &ld!(h,c)), // 0x61 LD H,C
        dop!("LD H,D"              , &ld!(h,d)), // 0x62 LD H,D
        dop!("LD H,E"              , &ld!(h,e)), // 0x63 LD H,E
        dop!("LD H,H"              , &ld!(h,h)), // 0x64 LD H,H
        dop!("LD H,L"              , &ld!(h,l)), // 0x65 LD H,L
        dop!("LD H,(HL)"           , &|cpu: &mut CPU| { cpu.regs.h = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x66 LD H,(HL)
        dop!("LD H,A"              , &ld!(h,a)), // 0x67 LD H,A
        dop!("LD L,B"              , &ld!(l,b)), // 0x68 LD L,B
        dop!("LD L,C"              , &ld!(l,c)), // 0x69 LD L,C
        dop!("LD L,D"              , &ld!(l,d)), // 0x6A LD L,D
        dop!("LD L,E"              , &ld!(l,e)), // 0x6B LD L,E
        dop!("LD L,H"              , &ld!(l,h)), // 0x6C LD L,H
        dop!("LD L,L"              , &ld!(l,l)), // 0x6D LD L,L
        dop!("LD L,(HL)"           , &|cpu: &mut CPU| { cpu.regs.l = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x6E LD L,(HL)
        dop!("LD L,A"              , &ld!(l,a)), // 0x6F LD L,A

        dop!("LD (HL),B"           , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.b); 2 }), // 0x70 LD (HL),B
        dop!("LD (HL),C"           , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.c); 2 }), // 0x71 LD (HL),C
        dop!("LD (HL),D"           , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.d); 2 }), // 0x72 LD (HL),D
        dop!("LD (HL),E"           , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.e); 2 }), // 0x73 LD (HL),E
        dop!("LD (HL),H"           , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.h); 2 }), // 0x74 LD (HL),H
        dop!("LD (HL),L"           , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.l); 2 }), // 0x75 LD (HL),L
        dop!("HALT"                , &CPU::unimplemented), // 0x76 HALT
        dop!("LD (HL),L"           , &|cpu: &mut CPU| { cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.a); 2 }), // 0x77 LD (HL),L
        dop!("LD A,B"              , &ld!(a,b)), // 0x78 LD A,B
        dop!("LD A,C"              , &ld!(a,c)), // 0x79 LD A,C
        dop!("LD A,D"              , &ld!(a,d)), // 0x7A LD A,D
        dop!("LD A,E"              , &ld!(a,e)), // 0x7B LD A,E
        dop!("LD A,H"              , &ld!(a,h)), // 0x7C LD A,H
        dop!("LD A,L"              , &ld!(a,l)), // 0x7D LD A,L
        dop!("LD A,(HL)"           , &|cpu: &mut CPU| { cpu.regs.a = cpu.mmu.read_byte(cpu.regs.get_hl()); 2 }), // 0x7E LD A,(HL)
        dop!("LD A,A"              , &ld!(a,a)), // 0x7F LD A,A

        dop!("ADD A,B"             , &alu_add!(a,b)), // 0x80 ADD A,B
        dop!("ADD A,C"             , &alu_add!(a,c)), // 0x81 ADD A,C
        dop!("ADD A,D"             , &alu_add!(a,d)), // 0x82 ADD A,D
        dop!("ADD A,E"             , &alu_add!(a,e)), // 0x83 ADD A,E
        dop!("ADD A,H"             , &alu_add!(a,h)), // 0x84 ADD A,H
        dop!("ADD A,L"             , &alu_add!(a,l)), // 0x85 ADD A,L
        dop!("ADD A,(HL)"          , &|cpu: &mut CPU| { let a = cpu.regs.a; let value = cpu.mmu.read_byte(cpu.regs.get_hl()); cpu.regs.a = cpu.alu_add_byte(a, value); 2 }), // 0x86 ADD A,(HL)
        dop!("ADD A,A"             , &alu_add!(a,a)), // 0x87 ADD A,A
        dop!("ADC A,B"             , &alu_adc!(a,b)), // 0x88 ADC A,B
        dop!("ADC A,C"             , &alu_adc!(a,c)), // 0x89 ADC A,C
        dop!("ADC A,D"             , &alu_adc!(a,d)), // 0x8A ADC A,D
        dop!("ADC A,E"             , &alu_adc!(a,e)), // 0x8B ADC A,E
        dop!("ADC A,H"             , &alu_adc!(a,h)), // 0x8C ADC A,H
        dop!("ADC A,L"             , &alu_adc!(a,l)), // 0x8D ADC A,L
        dop!("ADC A,(HL)"          , &|cpu: &mut CPU| { let a = cpu.regs.a; let value = cpu.mmu.read_byte(cpu.regs.get_hl()); cpu.regs.a = cpu.alu_add_byte_with_carry(a, value); 2 }), // 0x8E ADC A,(HL)
        dop!("ADC A,A"             , &alu_adc!(a,a)), // 0x8F ADC A,A

        dop!("SUB A,B"             , &alu_sub!(a,b)),
        dop!("SUB A,C"             , &alu_sub!(a,c)),
        dop!("SUB A,D"             , &alu_sub!(a,d)),
        dop!("SUB A,E"             , &alu_sub!(a,e)),
        dop!("SUB A,H"             , &alu_sub!(a,h)),
        dop!("SUB A,L"             , &alu_sub!(a,l)),
        dop!("SUB A,(HL)"          , &|cpu: &mut CPU| { let a = cpu.regs.a; let value = cpu.mmu.read_byte(cpu.regs.get_hl()); cpu.regs.a = cpu.alu_sub_byte(a, value); 2 }),
        dop!("SUB A,A"             , &alu_sub!(a,a)),
        dop!("SBC A,B"             , &alu_sbc!(a,b)),
        dop!("SBC A,C"             , &alu_sbc!(a,c)),
        dop!("SBC A,D"             , &alu_sbc!(a,d)),
        dop!("SBC A,E"             , &alu_sbc!(a,e)),
        dop!("SBC A,H"             , &alu_sbc!(a,h)),
        dop!("SBC A,L"             , &alu_sbc!(a,l)),
        dop!("SBC A,(HL)"          , &|cpu: &mut CPU| { let a = cpu.regs.a; let value = cpu.mmu.read_byte(cpu.regs.get_hl()); cpu.regs.a = cpu.alu_sub_byte_with_carry(a, value); 2 }),
        dop!("SBC A,A"             , &alu_sbc!(a,a)),

        dop!("AND A,B"             , &alu_and!(a,b)),
        dop!("AND A,C"             , &alu_and!(a,c)),
        dop!("AND A,D"             , &alu_and!(a,d)),
        dop!("AND A,E"             , &alu_and!(a,e)),
        dop!("AND A,H"             , &alu_and!(a,h)),
        dop!("AND A,L"             , &alu_and!(a,l)),
        dop!("AND A,(HL)"          , &|cpu: &mut CPU| { let a = cpu.regs.a; let value = cpu.mmu.read_byte(cpu.regs.get_hl()); cpu.regs.a = cpu.alu_and(a, value); 2 }),
        dop!("AND A,A"             , &alu_xor!(a,a)),
        dop!("XOR A,B"             , &alu_xor!(a,b)),
        dop!("XOR A,C"             , &alu_xor!(a,c)),
        dop!("XOR A,D"             , &alu_xor!(a,d)),
        dop!("XOR A,E"             , &alu_xor!(a,e)),
        dop!("XOR A,H"             , &alu_xor!(a,h)),
        dop!("XOR A,L"             , &alu_xor!(a,l)),
        dop!("XOR A,(HL)"          , &|cpu: &mut CPU| { let a = cpu.regs.a; let value = cpu.mmu.read_byte(cpu.regs.get_hl()); cpu.regs.a = cpu.alu_xor(a, value); 2 }),
        dop!("XOR A,A"             , &alu_xor!(a,a)),

        dop!("OR A,B"              , &alu_or!(a,b)),
        dop!("OR A,C"              , &alu_or!(a,c)),
        dop!("OR A,D"              , &alu_or!(a,d)),
        dop!("OR A,E"              , &alu_or!(a,e)),
        dop!("OR A,H"              , &alu_or!(a,h)),
        dop!("OR A,L"              , &alu_or!(a,l)),
        dop!("OR A,(HL)"           , &|cpu: &mut CPU| { let a = cpu.regs.a; let value = cpu.mmu.read_byte(cpu.regs.get_hl()); cpu.regs.a = cpu.alu_or(a, value); 2 }),
        dop!("OR A,A"              , &alu_or!(a,a)),
        dop!("CP A,B"              , &alu_cp!(a,b)),
        dop!("CP A,C"              , &alu_cp!(a,c)),
        dop!("CP A,D"              , &alu_cp!(a,d)),
        dop!("CP A,E"              , &alu_cp!(a,e)),
        dop!("CP A,H"              , &alu_cp!(a,h)),
        dop!("CP A,L"              , &alu_cp!(a,l)),
        dop!("CP A,(HL)"           , &|cpu: &mut CPU| { let a = cpu.regs.a; let value = cpu.mmu.read_byte(cpu.regs.get_hl()); cpu.alu_cp(a, value); 2 }),
        dop!("CP A,A"              , &alu_cp!(a,a)),

        dop!("RET NZ"              , &CPU::stack_ret_nz),
        dop!("POP BC"              , &|cpu: &mut CPU| { let value = cpu.stack_pop(); cpu.regs.set_bc(value); 3 }),
        dop!("JP NZ,{:#06X}"  , u16, &CPU::jump_nz),
        dop!("JP {:#06X}"     , u16, &CPU::jump),
        dop!("CALL NZ,{:#06X}", u16, &CPU::stack_call_nz),
        dop!("PUSH BC"             , &CPU::stack_push_bc),
        dop!("ADD A,{:#04X}"  , u8 , &|cpu: &mut CPU, value| { let a = cpu.regs.a; cpu.regs.a = cpu.alu_add_byte(a, value); 2 }),
        dop!("RST 00H"             , &CPU::unimplemented),
        dop!("RET Z"               , &CPU::stack_ret_z),
        dop!("RET"                 , &CPU::stack_ret), // 0xC9 RET
        dop!("JP Z,{:#06X}"   , u16, &CPU::jump_z),
        dop!("PREFIX CB"           , &CPU::process_cb),
        dop!("CALL Z,{:#06X}" , u16, &CPU::stack_call_z),
        dop!("CALL {:#06X}"   , u16, &CPU::stack_call),
        dop!("ADC A,{:#04X}"  , u8 , &|cpu: &mut CPU, value| { let a = cpu.regs.a; cpu.regs.a = cpu.alu_add_byte_with_carry(a, value); 2 }),
        dop!("RST 08H"             , &CPU::unimplemented),

        dop!("RET NC"              , &CPU::stack_ret_nc),
        dop!("POP DE"              , &|cpu: &mut CPU| { let value = cpu.stack_pop(); cpu.regs.set_de(value); 3 }),
        dop!("JP NC,{:#06X}"  , u16, &CPU::jump_nc),
        dop!("0xD3 NOPE"           , &CPU::nonexistant),
        dop!("CALL NC,{:#06X}", u16, &CPU::stack_call_nc),
        dop!("PUSH DE"             , &CPU::stack_push_de),
        dop!("SUB A,{:#04X}"  , u8 , &|cpu: &mut CPU, value| { let a = cpu.regs.a; cpu.regs.a = cpu.alu_sub_byte(a, value); 2 }),
        dop!("RST 10H"             , &CPU::unimplemented),
        dop!("RET C"               , &CPU::stack_ret_c),
        dop!("RETI"                , &CPU::unimplemented),
        dop!("JP C,{:#06X}"   , u16, &CPU::jump_c),
        dop!("0xDB NOPE"           , &CPU::nonexistant),
        dop!("CALL C,{:#06X}" , u16, &CPU::stack_call_c),
        dop!("0xDD NOPE"           , &CPU::nonexistant),
        dop!("SBC A,{:#04X}"  , u8 , &CPU::unimplemented_8),
        dop!("RST 18H"             , &CPU::unimplemented),

        dop!("LDH ({:#04X}),A", u8 , &|cpu: &mut CPU, addr| { let value = cpu.regs.a; cpu.mmu.write_byte(addr as u16 + 0xFF00, value); 3 }),
        dop!("POP HL"              , &|cpu: &mut CPU| { let value = cpu.stack_pop(); cpu.regs.set_hl(value); 3 }),
        dop!("LD (C),A"            , &|cpu: &mut CPU| { let addr = cpu.regs.c; let value = cpu.regs.a; cpu.mmu.write_byte(addr as u16 + 0xFF00, value); 2 }),
        dop!("0xE3 NOPE"           , &CPU::nonexistant),
        dop!("0xE4 NOPE"           , &CPU::nonexistant),
        dop!("PUSH HL"             , &CPU::stack_push_hl),
        dop!("AND A,{:#04X}"  , u8 , &|cpu: &mut CPU, value| { cpu.regs.a = value; 2 }),
        dop!("RST 20H"             , &CPU::unimplemented),
        dop!("ADD SP,{:#04X}" , u8 , &CPU::unimplemented_8),
        dop!("JP (HL)"             , &|cpu: &mut CPU| { cpu.regs.pc = cpu.mmu.read_word(cpu.regs.get_hl()); 1 }),
        dop!("LD ({:#06X}),A" , u16, &|cpu: &mut CPU, addr| { cpu.mmu.write_byte(addr, cpu.regs.a); 4 }),
        dop!("0xEB NOPE"           , &CPU::nonexistant),
        dop!("0xEC NOPE"           , &CPU::nonexistant),
        dop!("0xED NOPE"           , &CPU::nonexistant),
        dop!("XOR A,{:#04X}"  , u8 , &CPU::unimplemented_8),
        dop!("RST 28H"             , &CPU::unimplemented),

        dop!("LDH A,({:#04X})", u8 , &|cpu: &mut CPU, addr| { cpu.regs.a = cpu.mmu.read_byte(addr as u16 + 0xFF00); 3 }),
        dop!("POP AF"              , &|cpu: &mut CPU| { let value = cpu.stack_pop(); cpu.regs.set_af(value); 3 }),
        dop!("LD A,(C)"            , &|cpu: &mut CPU| { let addr = cpu.regs.c; cpu.regs.a = cpu.mmu.read_byte(addr as u16 + 0xFF00); 2 }),
        dop!("DI"                  , &CPU::enable_interupts),
        dop!("0xF4 NOPE"           , &CPU::nonexistant),
        dop!("PUSH AF"             , &CPU::stack_push_af),
        dop!("OR {:#04X}"     , u8 , &CPU::unimplemented_8),
        dop!("RST 30H"             , &CPU::unimplemented),
        dop!("LD HL,SP+{:#04X}", u8, &CPU::unimplemented_8),
        dop!("LD SP,HL"            , &CPU::unimplemented),
        dop!("LD A,({:#06X})" , u16, &|cpu: &mut CPU, addr| { cpu.regs.a = cpu.mmu.read_byte(addr); 4 }),
        dop!("DI"                  , &CPU::disable_interupts),
        dop!("0xFC NOPE"           , &CPU::nonexistant),
        dop!("0xFD NOPE"           , &CPU::nonexistant),
        dop!("CP A,{:#04X}"   , u8 , &|cpu: &mut CPU, value| { let a = cpu.regs.a; cpu.alu_cp(a, value); 2 }),
        dop!("RST 38H"             , &CPU::unimplemented),
    ];

    const CB_OPS: &'static [Operation] = &[
        dop!("RLC B"                 , &CPU::unimplemented),
        dop!("RLC C"                 , &CPU::unimplemented),
        dop!("RLC D"                 , &CPU::unimplemented),
        dop!("RLC E"                 , &CPU::unimplemented),
        dop!("RLC H"                 , &CPU::unimplemented),
        dop!("RLC L"                 , &CPU::unimplemented),
        dop!("RLC (HL)"              , &CPU::unimplemented),
        dop!("RLC A"                 , &CPU::unimplemented),
        dop!("RRC B"                 , &CPU::unimplemented),
        dop!("RRC C"                 , &CPU::unimplemented),
        dop!("RRC D"                 , &CPU::unimplemented),
        dop!("RRC E"                 , &CPU::unimplemented),
        dop!("RRC H"                 , &CPU::unimplemented),
        dop!("RRC L"                 , &CPU::unimplemented),
        dop!("RRC (HL)"              , &CPU::unimplemented),
        dop!("RRC A"                 , &CPU::unimplemented),

        dop!("RL B"                  , &CPU::unimplemented),
        dop!("RL C"                  , &CPU::unimplemented),
        dop!("RL D"                  , &CPU::unimplemented),
        dop!("RL E"                  , &CPU::unimplemented),
        dop!("RL H"                  , &CPU::unimplemented),
        dop!("RL L"                  , &CPU::unimplemented),
        dop!("RL (HL)"               , &CPU::unimplemented),
        dop!("RL A"                  , &CPU::unimplemented),
        dop!("RR B"                  , &CPU::unimplemented),
        dop!("RR C"                  , &CPU::unimplemented),
        dop!("RR D"                  , &CPU::unimplemented),
        dop!("RR E"                  , &CPU::unimplemented),
        dop!("RR H"                  , &CPU::unimplemented),
        dop!("RR L"                  , &CPU::unimplemented),
        dop!("RR (HL)"               , &CPU::unimplemented),
        dop!("RR A"                  , &CPU::unimplemented),

        dop!("SLA B"                 , &CPU::unimplemented),
        dop!("SLA C"                 , &CPU::unimplemented),
        dop!("SLA D"                 , &CPU::unimplemented),
        dop!("SLA E"                 , &CPU::unimplemented),
        dop!("SLA H"                 , &CPU::unimplemented),
        dop!("SLA L"                 , &CPU::unimplemented),
        dop!("SLA (HL)"              , &CPU::unimplemented),
        dop!("SLA A"                 , &CPU::unimplemented),
        dop!("SRA B"                 , &CPU::unimplemented),
        dop!("SRA C"                 , &CPU::unimplemented),
        dop!("SRA D"                 , &CPU::unimplemented),
        dop!("SRA E"                 , &CPU::unimplemented),
        dop!("SRA H"                 , &CPU::unimplemented),
        dop!("SRA L"                 , &CPU::unimplemented),
        dop!("SRA (HL)"              , &CPU::unimplemented),
        dop!("SRA A"                 , &CPU::unimplemented),

        dop!("SWAP B"                , &CPU::unimplemented),
        dop!("SWAP C"                , &CPU::unimplemented),
        dop!("SWAP D"                , &CPU::unimplemented),
        dop!("SWAP E"                , &CPU::unimplemented),
        dop!("SWAP H"                , &CPU::unimplemented),
        dop!("SWAP L"                , &CPU::unimplemented),
        dop!("SWAP (HL)"             , &CPU::unimplemented),
        dop!("SWAP A"                , &CPU::unimplemented),
        dop!("SRL B"                 , &CPU::unimplemented),
        dop!("SRL C"                 , &CPU::unimplemented),
        dop!("SRL D"                 , &CPU::unimplemented),
        dop!("SRL E"                 , &CPU::unimplemented),
        dop!("SRL H"                 , &CPU::unimplemented),
        dop!("SRL L"                 , &CPU::unimplemented),
        dop!("SRL (HL)"              , &CPU::unimplemented),
        dop!("SRL A"                 , &CPU::unimplemented),

        dop!("BIT 0,B"                 , &CPU::unimplemented),
        dop!("BIT 0,C"                 , &CPU::unimplemented),
        dop!("BIT 0,D"                 , &CPU::unimplemented),
        dop!("BIT 0,E"                 , &CPU::unimplemented),
        dop!("BIT 0,H"                 , &CPU::unimplemented),
        dop!("BIT 0,L"                 , &CPU::unimplemented),
        dop!("BIT 0,(HL)"              , &CPU::unimplemented),
        dop!("BIT 0,A"                 , &CPU::unimplemented),
        dop!("BIT 1,B"                 , &CPU::unimplemented),
        dop!("BIT 1,C"                 , &CPU::unimplemented),
        dop!("BIT 1,D"                 , &CPU::unimplemented),
        dop!("BIT 1,E"                 , &CPU::unimplemented),
        dop!("BIT 1,H"                 , &CPU::unimplemented),
        dop!("BIT 1,L"                 , &CPU::unimplemented),
        dop!("BIT 1,(HL)"              , &CPU::unimplemented),
        dop!("BIT 1,A"                 , &CPU::unimplemented),

        dop!("BIT 2,B"                 , &CPU::unimplemented),
        dop!("BIT 2,C"                 , &CPU::unimplemented),
        dop!("BIT 2,D"                 , &CPU::unimplemented),
        dop!("BIT 2,E"                 , &CPU::unimplemented),
        dop!("BIT 2,H"                 , &CPU::unimplemented),
        dop!("BIT 2,L"                 , &CPU::unimplemented),
        dop!("BIT 2,(HL)"              , &CPU::unimplemented),
        dop!("BIT 2,A"                 , &CPU::unimplemented),
        dop!("BIT 3,B"                 , &CPU::unimplemented),
        dop!("BIT 3,C"                 , &CPU::unimplemented),
        dop!("BIT 3,D"                 , &CPU::unimplemented),
        dop!("BIT 3,E"                 , &CPU::unimplemented),
        dop!("BIT 3,H"                 , &CPU::unimplemented),
        dop!("BIT 3,L"                 , &CPU::unimplemented),
        dop!("BIT 3,(HL)"              , &CPU::unimplemented),
        dop!("BIT 3,A"                 , &CPU::unimplemented),

        dop!("BIT 4,B"                 , &CPU::unimplemented),
        dop!("BIT 4,C"                 , &CPU::unimplemented),
        dop!("BIT 4,D"                 , &CPU::unimplemented),
        dop!("BIT 4,E"                 , &CPU::unimplemented),
        dop!("BIT 4,H"                 , &CPU::unimplemented),
        dop!("BIT 4,L"                 , &CPU::unimplemented),
        dop!("BIT 4,(HL)"              , &CPU::unimplemented),
        dop!("BIT 4,A"                 , &CPU::unimplemented),
        dop!("BIT 5,B"                 , &CPU::unimplemented),
        dop!("BIT 5,C"                 , &CPU::unimplemented),
        dop!("BIT 5,D"                 , &CPU::unimplemented),
        dop!("BIT 5,E"                 , &CPU::unimplemented),
        dop!("BIT 5,H"                 , &CPU::unimplemented),
        dop!("BIT 5,L"                 , &CPU::unimplemented),
        dop!("BIT 5,(HL)"              , &CPU::unimplemented),
        dop!("BIT 5,A"                 , &CPU::unimplemented),

        dop!("BIT 6,B"                 , &CPU::unimplemented),
        dop!("BIT 6,C"                 , &CPU::unimplemented),
        dop!("BIT 6,D"                 , &CPU::unimplemented),
        dop!("BIT 6,E"                 , &CPU::unimplemented),
        dop!("BIT 6,H"                 , &CPU::unimplemented),
        dop!("BIT 6,L"                 , &CPU::unimplemented),
        dop!("BIT 6,(HL)"              , &CPU::unimplemented),
        dop!("BIT 6,A"                 , &CPU::unimplemented),
        dop!("BIT 7,B"                 , &CPU::unimplemented),
        dop!("BIT 7,C"                 , &CPU::unimplemented),
        dop!("BIT 7,D"                 , &CPU::unimplemented),
        dop!("BIT 7,E"                 , &CPU::unimplemented),
        dop!("BIT 7,H"                 , &CPU::unimplemented),
        dop!("BIT 7,L"                 , &CPU::unimplemented),
        dop!("BIT 7,(HL)"              , &CPU::unimplemented),
        dop!("BIT 7,A"                 , &CPU::unimplemented),

        dop!("RES 0,B"                 , &CPU::unimplemented),
        dop!("RES 0,C"                 , &CPU::unimplemented),
        dop!("RES 0,D"                 , &CPU::unimplemented),
        dop!("RES 0,E"                 , &CPU::unimplemented),
        dop!("RES 0,H"                 , &CPU::unimplemented),
        dop!("RES 0,L"                 , &CPU::unimplemented),
        dop!("RES 0,(HL)"              , &CPU::unimplemented),
        dop!("RES 0,A"                 , &CPU::unimplemented),
        dop!("RES 1,B"                 , &CPU::unimplemented),
        dop!("RES 1,C"                 , &CPU::unimplemented),
        dop!("RES 1,D"                 , &CPU::unimplemented),
        dop!("RES 1,E"                 , &CPU::unimplemented),
        dop!("RES 1,H"                 , &CPU::unimplemented),
        dop!("RES 1,L"                 , &CPU::unimplemented),
        dop!("RES 1,(HL)"              , &CPU::unimplemented),
        dop!("RES 1,A"                 , &CPU::unimplemented),

        dop!("RES 2,B"                 , &CPU::unimplemented),
        dop!("RES 2,C"                 , &CPU::unimplemented),
        dop!("RES 2,D"                 , &CPU::unimplemented),
        dop!("RES 2,E"                 , &CPU::unimplemented),
        dop!("RES 2,H"                 , &CPU::unimplemented),
        dop!("RES 2,L"                 , &CPU::unimplemented),
        dop!("RES 2,(HL)"              , &CPU::unimplemented),
        dop!("RES 2,A"                 , &CPU::unimplemented),
        dop!("RES 3,B"                 , &CPU::unimplemented),
        dop!("RES 3,C"                 , &CPU::unimplemented),
        dop!("RES 3,D"                 , &CPU::unimplemented),
        dop!("RES 3,E"                 , &CPU::unimplemented),
        dop!("RES 3,H"                 , &CPU::unimplemented),
        dop!("RES 3,L"                 , &CPU::unimplemented),
        dop!("RES 3,(HL)"              , &CPU::unimplemented),
        dop!("RES 3,A"                 , &CPU::unimplemented),

        dop!("RES 4,B"                 , &CPU::unimplemented),
        dop!("RES 4,C"                 , &CPU::unimplemented),
        dop!("RES 4,D"                 , &CPU::unimplemented),
        dop!("RES 4,E"                 , &CPU::unimplemented),
        dop!("RES 4,H"                 , &CPU::unimplemented),
        dop!("RES 4,L"                 , &CPU::unimplemented),
        dop!("RES 4,(HL)"              , &CPU::unimplemented),
        dop!("RES 4,A"                 , &CPU::unimplemented),
        dop!("RES 5,B"                 , &CPU::unimplemented),
        dop!("RES 5,C"                 , &CPU::unimplemented),
        dop!("RES 5,D"                 , &CPU::unimplemented),
        dop!("RES 5,E"                 , &CPU::unimplemented),
        dop!("RES 5,H"                 , &CPU::unimplemented),
        dop!("RES 5,L"                 , &CPU::unimplemented),
        dop!("RES 5,(HL)"              , &CPU::unimplemented),
        dop!("RES 5,A"                 , &CPU::unimplemented),

        dop!("RES 6,B"                 , &CPU::unimplemented),
        dop!("RES 6,C"                 , &CPU::unimplemented),
        dop!("RES 6,D"                 , &CPU::unimplemented),
        dop!("RES 6,E"                 , &CPU::unimplemented),
        dop!("RES 6,H"                 , &CPU::unimplemented),
        dop!("RES 6,L"                 , &CPU::unimplemented),
        dop!("RES 6,(HL)"              , &CPU::unimplemented),
        dop!("RES 6,A"                 , &CPU::unimplemented),
        dop!("RES 7,B"                 , &CPU::unimplemented),
        dop!("RES 7,C"                 , &CPU::unimplemented),
        dop!("RES 7,D"                 , &CPU::unimplemented),
        dop!("RES 7,E"                 , &CPU::unimplemented),
        dop!("RES 7,H"                 , &CPU::unimplemented),
        dop!("RES 7,L"                 , &CPU::unimplemented),
        dop!("RES 7,(HL)"              , &CPU::unimplemented),
        dop!("RES 7,A"                 , &CPU::unimplemented),

        dop!("SET 0,B"                 , &CPU::unimplemented),
        dop!("SET 0,C"                 , &CPU::unimplemented),
        dop!("SET 0,D"                 , &CPU::unimplemented),
        dop!("SET 0,E"                 , &CPU::unimplemented),
        dop!("SET 0,H"                 , &CPU::unimplemented),
        dop!("SET 0,L"                 , &CPU::unimplemented),
        dop!("SET 0,(HL)"              , &CPU::unimplemented),
        dop!("SET 0,A"                 , &CPU::unimplemented),
        dop!("SET 1,B"                 , &CPU::unimplemented),
        dop!("SET 1,C"                 , &CPU::unimplemented),
        dop!("SET 1,D"                 , &CPU::unimplemented),
        dop!("SET 1,E"                 , &CPU::unimplemented),
        dop!("SET 1,H"                 , &CPU::unimplemented),
        dop!("SET 1,L"                 , &CPU::unimplemented),
        dop!("SET 1,(HL)"              , &CPU::unimplemented),
        dop!("SET 1,A"                 , &CPU::unimplemented),

        dop!("SET 2,B"                 , &CPU::unimplemented),
        dop!("SET 2,C"                 , &CPU::unimplemented),
        dop!("SET 2,D"                 , &CPU::unimplemented),
        dop!("SET 2,E"                 , &CPU::unimplemented),
        dop!("SET 2,H"                 , &CPU::unimplemented),
        dop!("SET 2,L"                 , &CPU::unimplemented),
        dop!("SET 2,(HL)"              , &CPU::unimplemented),
        dop!("SET 2,A"                 , &CPU::unimplemented),
        dop!("SET 3,B"                 , &CPU::unimplemented),
        dop!("SET 3,C"                 , &CPU::unimplemented),
        dop!("SET 3,D"                 , &CPU::unimplemented),
        dop!("SET 3,E"                 , &CPU::unimplemented),
        dop!("SET 3,H"                 , &CPU::unimplemented),
        dop!("SET 3,L"                 , &CPU::unimplemented),
        dop!("SET 3,(HL)"              , &CPU::unimplemented),
        dop!("SET 3,A"                 , &CPU::unimplemented),

        dop!("SET 4,B"                 , &CPU::unimplemented),
        dop!("SET 4,C"                 , &CPU::unimplemented),
        dop!("SET 4,D"                 , &CPU::unimplemented),
        dop!("SET 4,E"                 , &CPU::unimplemented),
        dop!("SET 4,H"                 , &CPU::unimplemented),
        dop!("SET 4,L"                 , &CPU::unimplemented),
        dop!("SET 4,(HL)"              , &CPU::unimplemented),
        dop!("SET 4,A"                 , &CPU::unimplemented),
        dop!("SET 5,B"                 , &CPU::unimplemented),
        dop!("SET 5,C"                 , &CPU::unimplemented),
        dop!("SET 5,D"                 , &CPU::unimplemented),
        dop!("SET 5,E"                 , &CPU::unimplemented),
        dop!("SET 5,H"                 , &CPU::unimplemented),
        dop!("SET 5,L"                 , &CPU::unimplemented),
        dop!("SET 5,(HL)"              , &CPU::unimplemented),
        dop!("SET 5,A"                 , &CPU::unimplemented),

        dop!("SET 6,B"                 , &CPU::unimplemented),
        dop!("SET 6,C"                 , &CPU::unimplemented),
        dop!("SET 6,D"                 , &CPU::unimplemented),
        dop!("SET 6,E"                 , &CPU::unimplemented),
        dop!("SET 6,H"                 , &CPU::unimplemented),
        dop!("SET 6,L"                 , &CPU::unimplemented),
        dop!("SET 6,(HL)"              , &CPU::unimplemented),
        dop!("SET 6,A"                 , &CPU::unimplemented),
        dop!("SET 7,B"                 , &CPU::unimplemented),
        dop!("SET 7,C"                 , &CPU::unimplemented),
        dop!("SET 7,D"                 , &CPU::unimplemented),
        dop!("SET 7,E"                 , &CPU::unimplemented),
        dop!("SET 7,H"                 , &CPU::unimplemented),
        dop!("SET 7,L"                 , &CPU::unimplemented),
        dop!("SET 7,(HL)"              , &CPU::unimplemented),
        dop!("SET 7,A"                 , &CPU::unimplemented),
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

        // let time = time::Duration::from_millis(100);
        // thread::sleep(time);

        let ticks = op_impl(self);

        println!("af: {:#06X}, bc: {:#06X}, de: {:#06X}, hl: {:#06X}, sp: {:#06X}, pc: {:#06X}",
            self.regs.get_af(),
            self.regs.get_bc(),
            self.regs.get_de(),
            self.regs.get_hl(),
            self.regs.sp,
            self.regs.pc
        );

        return ticks;
    }

    fn process_cb(&mut self) -> usize {
        let op_code = self.fetch_byte();
        let op = &CPU::CB_OPS[op_code as usize];
        let op_impl = op.execute;

        let ticks = op_impl(self);

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

    fn stop(&mut self) -> usize {
       println!("STOP not implemented");
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

    // DEBUG

    pub fn print_reg_state(&self) {
        println!("Hello, world! {:?}", self.regs);
    }
}
