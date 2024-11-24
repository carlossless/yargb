use mmu::MMU;
use registers::Registers;
use registers::WordRegister::{BC, DE, HL, SP};
// use utils::format::FormatAsSigned;

#[macro_use]
mod load;
#[macro_use]
mod alu;
mod jump;
#[macro_use]
mod stack;
#[macro_use]
mod shift;

pub struct CPU {
    regs: Registers,
    mmu: MMU,
    pub halted: bool,
}

struct Operation {
    mneumonic: &'static str,
    size: usize,
    execute: &'static dyn Fn(&mut CPU) -> usize,
}

macro_rules! dop {
    ($name:tt, $execute:expr) => {
        Operation {
            mneumonic: $name,
            size: 1,
            execute: &|cpu| {
                // print!($name);
                $execute(cpu)
            },
        }
    };
    ($name:tt, u8, $execute:expr) => {
        Operation {
            mneumonic: $name,
            size: 2,
            execute: &|cpu: &mut CPU| {
                let value: u8 = cpu.fetch_byte();
                // print!($name, value);
                $execute(cpu, value)
            },
        }
    };
    ($name:tt, i8, $execute:expr) => {
        Operation {
            mneumonic: $name,
            size: 2,
            execute: &|cpu: &mut CPU| {
                let value = cpu.fetch_byte() as i8;
                // print!($name, FormatAsSigned(value));
                $execute(cpu, value)
            },
        }
    };
    ($name:tt, u16, $execute:expr) => {
        Operation {
            mneumonic: $name,
            size: 3,
            execute: &|cpu: &mut CPU| {
                let value: u16 = cpu.fetch_word();
                // print!($name, value);
                $execute(cpu, value)
            },
        }
    };
}

impl CPU {
    const OPS: &'static [Operation] = &[
        dop!("NOP", &CPU::nop), // 0x00 NOP
        dop!("LD BC,{:#06X}", u16, &|cpu: &mut CPU, value| {
            cpu.regs.set_bc(value);
            3
        }), // 0x01 LD BC,d16
        dop!("LD (BC),A", &|cpu: &mut CPU| {
            cpu.mmu.write_byte(cpu.regs.get_bc(), cpu.regs.a);
            2
        }), // 0x02 LD (BC),A
        dop!("INC BC", &alu_inc_word!(BC)), // 0x03 INC BC
        dop!("INC B", &alu_inc_byte!(b)), // 0x04 INC B
        dop!("DEC B", &alu_dec_byte!(b)), // 0x05 DEC B
        dop!("LD B,{:#04X}", u8, &load!(b)), // 0x06 LD B,d8
        dop!("RLCA", &CPU::rotate_left_circular_accumulator), // 0x07 RLCA
        dop!("LD ({:#06X}),SP", u16, &|cpu: &mut CPU, addr| {
            cpu.mmu.write_word(addr, cpu.regs.sp);
            5
        }), // 0x08 LD (a16),SP
        dop!("ADD HL,BC", &|cpu: &mut CPU| {
            let v = cpu.regs.get_bc();
            cpu.alu_add_to_hl(v);
            2
        }), // 0x09 ADD HL,BC
        dop!("LD A,(BC)", &|cpu: &mut CPU| {
            let a = cpu.regs.get_bc();
            cpu.regs.a = cpu.mmu.read_byte(a);
            2
        }), // 0x0A LD A,(BC)
        dop!("DEC BC", &alu_dec_word!(BC)), // 0x0B DEC BC
        dop!("INC C", &alu_inc_byte!(c)), // 0x0C INC C
        dop!("DEC C", &alu_dec_byte!(c)), // 0x0D DEC C
        dop!("LD C,{:#04X}", u8, &load!(c)), // 0x0E LD C,d8
        dop!("RRCA", &CPU::rotate_right_circular_accumulator), // 0x0F RRCA
        dop!("STOP", &CPU::stop), // 0x10 STOP
        dop!("LD DE,{:#06X}", u16, &|cpu: &mut CPU, value| {
            cpu.regs.set_de(value);
            3
        }), // 0x11 LD DE,d16
        dop!("LD (DE),A", &|cpu: &mut CPU| {
            cpu.mmu.write_byte(cpu.regs.get_de(), cpu.regs.a);
            2
        }), // 0x12 LD (DE),A
        dop!("INC DE", &alu_inc_word!(DE)), // 0x13 INC DE
        dop!("INC D", &alu_inc_byte!(d)), // 0x14 INC D
        dop!("DEC D", &alu_dec_byte!(d)), // 0x15 DEC D
        dop!("LD D,{:#04X}", u8, &load!(d)), // 0x16 LD D,d8
        dop!("RLA", &CPU::rotate_left_accumulator), // 0x07 RLA
        dop!("JR {:#02X}", i8, &CPU::relative_jump), // 0x18 JR r8
        dop!("ADD HL,DE", &|cpu: &mut CPU| {
            let v = cpu.regs.get_de();
            cpu.alu_add_to_hl(v);
            2
        }), // 0x19 ADD HL,DE
        dop!("LD A,(DE)", &|cpu: &mut CPU| {
            let a = cpu.regs.get_de();
            cpu.regs.a = cpu.mmu.read_byte(a);
            2
        }), // 0x1A LD A,(DE)
        dop!("DEC DE", &alu_dec_word!(DE)), // 0x1B DEC DE
        dop!("INC E", &alu_inc_byte!(e)), // 0x1C INC E
        dop!("DEC E", &alu_dec_byte!(e)), // 0x1D DEC E
        dop!("LD E,{:#04X}", u8, &load!(e)), // 0x1E LD E,d8
        dop!("RRA", &CPU::rotate_right_accumulator), // 0x1F RRA
        dop!("JR NZ,{:#02X}", i8, &CPU::relative_jump_nz), // 0x20 JR NZ,r8
        dop!("LD HL,{:#06X}", u16, &|cpu: &mut CPU, value| {
            cpu.regs.set_hl(value);
            3
        }), // 0x21 LD HL,d16
        dop!("LD (HL+),A", &|cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            cpu.mmu.write_byte(addr, cpu.regs.a);
            cpu.regs.set_hl(addr + 1);
            2
        }), // 0x22 LD (HL+),A
        dop!("INC HL", &alu_inc_word!(HL)), // 0x23 INC HL
        dop!("INC H", &alu_inc_byte!(h)), // 0x24 INC H
        dop!("DEC H", &alu_dec_byte!(h)), // 0x25 DEC H
        dop!("LD H,{:#04X}", u8, &load!(h)), // 0x26 LD H,d8
        dop!("DAA", &CPU::daa), // 0x27 DAA
        dop!("JR Z,{:#04X}", i8, &CPU::relative_jump_z), // 0x28 JR Z,r8
        dop!("ADD HL,HL", &|cpu: &mut CPU| {
            let v = cpu.regs.get_hl();
            cpu.alu_add_to_hl(v);
            2
        }), // 0x29 ADD HL,HL
        dop!("LD A,(HL+)", &|cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            cpu.regs.a = cpu.mmu.read_byte(addr);
            cpu.regs.set_hl(addr + 1);
            2
        }), // 0x2A LD A,(HL+)
        dop!("DEC HL", &alu_dec_word!(HL)), // 0x2B DEC HL
        dop!("INC L", &alu_inc_byte!(l)), // 0x2C INC L
        dop!("DEC L", &alu_dec_byte!(l)), // 0x2D DEC L
        dop!("LD L,{:#04X}", u8, &load!(l)), // 0x2E LD L,d8
        dop!("CPL", &CPU::alu_complement), // 0x2F CPL
        dop!("JR NC,{:#04X}", i8, &CPU::relative_jump_nc), // 0x30 JR NC,r8
        dop!("LD SP,{:#06X}", u16, &|cpu: &mut CPU, value| {
            cpu.regs.sp = value;
            3
        }), // 0x31 LD SP,d16
        dop!("LD (HL-),A", &|cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            cpu.mmu.write_byte(addr, cpu.regs.a);
            cpu.regs.set_hl(addr - 1);
            2
        }), // 0x32 LD (HL-),A
        dop!("INC SP", &alu_inc_word!(SP)), // 0x33 INC SP
        dop!("INC (HL)", &|cpu: &mut CPU| {
            let a = cpu.regs.get_hl();
            let mut v = cpu.mmu.read_byte(a);
            cpu.alu_inc_byte(&mut v);
            cpu.mmu.write_byte(a, v);
            3
        }), // 0x34 INC (HL)
        dop!("DEC (HL)", &|cpu: &mut CPU| {
            let a = cpu.regs.get_hl();
            let mut v = cpu.mmu.read_byte(a);
            cpu.alu_dec_byte(&mut v);
            cpu.mmu.write_byte(a, v);
            3
        }), // 0x35 DEC (HL)
        dop!("LD (HL),{:#04X}", u8, &|cpu: &mut CPU, value| {
            let a = cpu.regs.get_hl();
            cpu.mmu.write_byte(a, value);
            3
        }), // 0x36 LD (HL),d8
        dop!("SCF", &CPU::alu_set_carry_flag), // 0x37 SCF
        dop!("JR C,{:#04X}", i8, &CPU::relative_jump_c), // 0x38 JR C,r8
        dop!("ADD HL,SP", &|cpu: &mut CPU| {
            let v = cpu.regs.sp;
            cpu.alu_add_to_hl(v);
            2
        }), // 0x39 ADD HL,SP
        dop!("LD A,(HL-)", &|cpu: &mut CPU| {
            let addr = cpu.regs.get_hl();
            cpu.regs.a = cpu.mmu.read_byte(addr);
            cpu.regs.set_hl(addr - 1);
            2
        }), // 0x3A LD A,(HL-)
        dop!("DEC SP", &alu_dec_word!(SP)), // 0x3B DEC SP
        dop!("INC A", &alu_inc_byte!(a)), // 0x3C INC A
        dop!("DEC A", &alu_dec_byte!(a)), // 0x3D DEC A
        dop!("LD A,{:#04X}", u8, &load!(a)), // 0x3E LD A,d8
        dop!("CCF", &CPU::alu_complement_carry_flag), // 0x3F CCF
        dop!("LD B,B", &load!(b, b)), // 0x40 LD B,B
        dop!("LD B,C", &load!(b, c)), // 0x41 LD B,C
        dop!("LD B,D", &load!(b, d)), // 0x42 LD B,D
        dop!("LD B,E", &load!(b, e)), // 0x43 LD B,E
        dop!("LD B,H", &load!(b, h)), // 0x44 LD B,H
        dop!("LD B,L", &load!(b, l)), // 0x45 LD B,L
        dop!("LD B,(HL)", &|cpu: &mut CPU| {
            cpu.regs.b = cpu.mmu.read_byte(cpu.regs.get_hl());
            2
        }), // 0x46 LD B,(HL)
        dop!("LD B,A", &load!(b, a)), // 0x47 LD B,A
        dop!("LD C,B", &load!(c, b)), // 0x48 LD C,B
        dop!("LD C,C", &load!(c, c)), // 0x49 LD C,C
        dop!("LD C,D", &load!(c, d)), // 0x4A LD C,D
        dop!("LD C,E", &load!(c, e)), // 0x4B LD C,E
        dop!("LD C,H", &load!(c, h)), // 0x4C LD C,H
        dop!("LD C,L", &load!(c, l)), // 0x4D LD C,L
        dop!("LD C,(HL)", &|cpu: &mut CPU| {
            cpu.regs.c = cpu.mmu.read_byte(cpu.regs.get_hl());
            2
        }), // 0x4E LD C,(HL)
        dop!("LD C,A", &load!(c, a)), // 0x4F LD C,A
        dop!("LD D,B", &load!(d, b)), // 0x50 LD B,B
        dop!("LD D,C", &load!(d, c)), // 0x51 LD B,C
        dop!("LD D,D", &load!(d, d)), // 0x52 LD B,D
        dop!("LD D,E", &load!(d, e)), // 0x53 LD B,E
        dop!("LD D,H", &load!(d, h)), // 0x54 LD B,H
        dop!("LD D,L", &load!(d, l)), // 0x55 LD B,L
        dop!("LD D,(HL)", &|cpu: &mut CPU| {
            cpu.regs.d = cpu.mmu.read_byte(cpu.regs.get_hl());
            2
        }), // 0x56 LD D,(HL)
        dop!("LD D,A", &load!(d, a)), // 0x57 LD B,A
        dop!("LD E,B", &load!(e, b)), // 0x58 LD C,B
        dop!("LD E,C", &load!(e, c)), // 0x59 LD C,C
        dop!("LD E,D", &load!(e, d)), // 0x5A LD C,D
        dop!("LD E,E", &load!(e, e)), // 0x5B LD C,E
        dop!("LD E,H", &load!(e, h)), // 0x5C LD C,H
        dop!("LD E,L", &load!(e, l)), // 0x5D LD C,L
        dop!("LD E,(HL)", &|cpu: &mut CPU| {
            cpu.regs.e = cpu.mmu.read_byte(cpu.regs.get_hl());
            2
        }), // 0x5E LD E,(HL)
        dop!("LD E,A", &load!(e, a)), // 0x5F LD C,A
        dop!("LD H,B", &load!(h, b)), // 0x60 LD H,B
        dop!("LD H,C", &load!(h, c)), // 0x61 LD H,C
        dop!("LD H,D", &load!(h, d)), // 0x62 LD H,D
        dop!("LD H,E", &load!(h, e)), // 0x63 LD H,E
        dop!("LD H,H", &load!(h, h)), // 0x64 LD H,H
        dop!("LD H,L", &load!(h, l)), // 0x65 LD H,L
        dop!("LD H,(HL)", &|cpu: &mut CPU| {
            cpu.regs.h = cpu.mmu.read_byte(cpu.regs.get_hl());
            2
        }), // 0x66 LD H,(HL)
        dop!("LD H,A", &load!(h, a)), // 0x67 LD H,A
        dop!("LD L,B", &load!(l, b)), // 0x68 LD L,B
        dop!("LD L,C", &load!(l, c)), // 0x69 LD L,C
        dop!("LD L,D", &load!(l, d)), // 0x6A LD L,D
        dop!("LD L,E", &load!(l, e)), // 0x6B LD L,E
        dop!("LD L,H", &load!(l, h)), // 0x6C LD L,H
        dop!("LD L,L", &load!(l, l)), // 0x6D LD L,L
        dop!("LD L,(HL)", &|cpu: &mut CPU| {
            cpu.regs.l = cpu.mmu.read_byte(cpu.regs.get_hl());
            2
        }), // 0x6E LD L,(HL)
        dop!("LD L,A", &load!(l, a)), // 0x6F LD L,A
        dop!("LD (HL),B", &|cpu: &mut CPU| {
            cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.b);
            2
        }), // 0x70 LD (HL),B
        dop!("LD (HL),C", &|cpu: &mut CPU| {
            cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.c);
            2
        }), // 0x71 LD (HL),C
        dop!("LD (HL),D", &|cpu: &mut CPU| {
            cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.d);
            2
        }), // 0x72 LD (HL),D
        dop!("LD (HL),E", &|cpu: &mut CPU| {
            cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.e);
            2
        }), // 0x73 LD (HL),E
        dop!("LD (HL),H", &|cpu: &mut CPU| {
            cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.h);
            2
        }), // 0x74 LD (HL),H
        dop!("LD (HL),L", &|cpu: &mut CPU| {
            cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.l);
            2
        }), // 0x75 LD (HL),L
        dop!("HALT", &CPU::halt), // 0x76 HALT
        dop!("LD (HL),L", &|cpu: &mut CPU| {
            cpu.mmu.write_byte(cpu.regs.get_hl(), cpu.regs.a);
            2
        }), // 0x77 LD (HL),L
        dop!("LD A,B", &load!(a, b)), // 0x78 LD A,B
        dop!("LD A,C", &load!(a, c)), // 0x79 LD A,C
        dop!("LD A,D", &load!(a, d)), // 0x7A LD A,D
        dop!("LD A,E", &load!(a, e)), // 0x7B LD A,E
        dop!("LD A,H", &load!(a, h)), // 0x7C LD A,H
        dop!("LD A,L", &load!(a, l)), // 0x7D LD A,L
        dop!("LD A,(HL)", &|cpu: &mut CPU| {
            cpu.regs.a = cpu.mmu.read_byte(cpu.regs.get_hl());
            2
        }), // 0x7E LD A,(HL)
        dop!("LD A,A", &load!(a, a)), // 0x7F LD A,A
        dop!("ADD A,B", &alu_add!(a, b)), // 0x80 ADD A,B
        dop!("ADD A,C", &alu_add!(a, c)), // 0x81 ADD A,C
        dop!("ADD A,D", &alu_add!(a, d)), // 0x82 ADD A,D
        dop!("ADD A,E", &alu_add!(a, e)), // 0x83 ADD A,E
        dop!("ADD A,H", &alu_add!(a, h)), // 0x84 ADD A,H
        dop!("ADD A,L", &alu_add!(a, l)), // 0x85 ADD A,L
        dop!("ADD A,(HL)", &|cpu: &mut CPU| {
            let a = cpu.regs.a;
            let value = cpu.mmu.read_byte(cpu.regs.get_hl());
            cpu.regs.a = cpu.alu_add_byte(a, value);
            2
        }), // 0x86 ADD A,(HL)
        dop!("ADD A,A", &alu_add!(a, a)), // 0x87 ADD A,A
        dop!("ADC A,B", &alu_adc!(a, b)), // 0x88 ADC A,B
        dop!("ADC A,C", &alu_adc!(a, c)), // 0x89 ADC A,C
        dop!("ADC A,D", &alu_adc!(a, d)), // 0x8A ADC A,D
        dop!("ADC A,E", &alu_adc!(a, e)), // 0x8B ADC A,E
        dop!("ADC A,H", &alu_adc!(a, h)), // 0x8C ADC A,H
        dop!("ADC A,L", &alu_adc!(a, l)), // 0x8D ADC A,L
        dop!("ADC A,(HL)", &|cpu: &mut CPU| {
            let a = cpu.regs.a;
            let value = cpu.mmu.read_byte(cpu.regs.get_hl());
            cpu.regs.a = cpu.alu_add_byte_with_carry(a, value);
            2
        }), // 0x8E ADC A,(HL)
        dop!("ADC A,A", &alu_adc!(a, a)), // 0x8F ADC A,A
        dop!("SUB A,B", &alu_sub!(a, b)),
        dop!("SUB A,C", &alu_sub!(a, c)),
        dop!("SUB A,D", &alu_sub!(a, d)),
        dop!("SUB A,E", &alu_sub!(a, e)),
        dop!("SUB A,H", &alu_sub!(a, h)),
        dop!("SUB A,L", &alu_sub!(a, l)),
        dop!("SUB A,(HL)", &|cpu: &mut CPU| {
            let a = cpu.regs.a;
            let value = cpu.mmu.read_byte(cpu.regs.get_hl());
            cpu.regs.a = cpu.alu_sub_byte(a, value);
            2
        }),
        dop!("SUB A,A", &alu_sub!(a, a)),
        dop!("SBC A,B", &alu_sbc!(a, b)),
        dop!("SBC A,C", &alu_sbc!(a, c)),
        dop!("SBC A,D", &alu_sbc!(a, d)),
        dop!("SBC A,E", &alu_sbc!(a, e)),
        dop!("SBC A,H", &alu_sbc!(a, h)),
        dop!("SBC A,L", &alu_sbc!(a, l)),
        dop!("SBC A,(HL)", &|cpu: &mut CPU| {
            let a = cpu.regs.a;
            let value = cpu.mmu.read_byte(cpu.regs.get_hl());
            cpu.regs.a = cpu.alu_sub_byte_with_carry(a, value);
            2
        }),
        dop!("SBC A,A", &alu_sbc!(a, a)),
        dop!("AND A,B", &alu_and!(a, b)),
        dop!("AND A,C", &alu_and!(a, c)),
        dop!("AND A,D", &alu_and!(a, d)),
        dop!("AND A,E", &alu_and!(a, e)),
        dop!("AND A,H", &alu_and!(a, h)),
        dop!("AND A,L", &alu_and!(a, l)),
        dop!("AND A,(HL)", &|cpu: &mut CPU| {
            let a = cpu.regs.a;
            let value = cpu.mmu.read_byte(cpu.regs.get_hl());
            cpu.regs.a = cpu.alu_and(a, value);
            2
        }),
        dop!("AND A,A", &alu_xor!(a, a)),
        dop!("XOR A,B", &alu_xor!(a, b)),
        dop!("XOR A,C", &alu_xor!(a, c)),
        dop!("XOR A,D", &alu_xor!(a, d)),
        dop!("XOR A,E", &alu_xor!(a, e)),
        dop!("XOR A,H", &alu_xor!(a, h)),
        dop!("XOR A,L", &alu_xor!(a, l)),
        dop!("XOR A,(HL)", &|cpu: &mut CPU| {
            let a = cpu.regs.a;
            let value = cpu.mmu.read_byte(cpu.regs.get_hl());
            cpu.regs.a = cpu.alu_xor(a, value);
            2
        }),
        dop!("XOR A,A", &alu_xor!(a, a)),
        dop!("OR A,B", &alu_or!(a, b)),
        dop!("OR A,C", &alu_or!(a, c)),
        dop!("OR A,D", &alu_or!(a, d)),
        dop!("OR A,E", &alu_or!(a, e)),
        dop!("OR A,H", &alu_or!(a, h)),
        dop!("OR A,L", &alu_or!(a, l)),
        dop!("OR A,(HL)", &|cpu: &mut CPU| {
            let a = cpu.regs.a;
            let value = cpu.mmu.read_byte(cpu.regs.get_hl());
            cpu.regs.a = cpu.alu_or(a, value);
            2
        }),
        dop!("OR A,A", &alu_or!(a, a)),
        dop!("CP A,B", &alu_cp!(a, b)),
        dop!("CP A,C", &alu_cp!(a, c)),
        dop!("CP A,D", &alu_cp!(a, d)),
        dop!("CP A,E", &alu_cp!(a, e)),
        dop!("CP A,H", &alu_cp!(a, h)),
        dop!("CP A,L", &alu_cp!(a, l)),
        dop!("CP A,(HL)", &|cpu: &mut CPU| {
            let a = cpu.regs.a;
            let value = cpu.mmu.read_byte(cpu.regs.get_hl());
            cpu.alu_cp(a, value);
            2
        }),
        dop!("CP A,A", &alu_cp!(a, a)),
        dop!("RET NZ", &CPU::stack_ret_nz),
        dop!("POP BC", &|cpu: &mut CPU| {
            let value = cpu.stack_pop();
            cpu.regs.set_bc(value);
            3
        }),
        dop!("JP NZ,{:#06X}", u16, &CPU::jump_nz),
        dop!("JP {:#06X}", u16, &CPU::jump),
        dop!("CALL NZ,{:#06X}", u16, &CPU::stack_call_nz),
        dop!("PUSH BC", &CPU::stack_push_bc),
        dop!("ADD A,{:#04X}", u8, &|cpu: &mut CPU, value| {
            let a = cpu.regs.a;
            cpu.regs.a = cpu.alu_add_byte(a, value);
            2
        }),
        dop!("RST 00H", &stack_rst!(0x00)),
        dop!("RET Z", &CPU::stack_ret_z),
        dop!("RET", &CPU::stack_ret), // 0xC9 RET
        dop!("JP Z,{:#06X}", u16, &CPU::jump_z),
        dop!("PREFIX CB", &CPU::process_cb),
        dop!("CALL Z,{:#06X}", u16, &CPU::stack_call_z),
        dop!("CALL {:#06X}", u16, &CPU::stack_call),
        dop!("ADC A,{:#04X}", u8, &|cpu: &mut CPU, value| {
            let a = cpu.regs.a;
            cpu.regs.a = cpu.alu_add_byte_with_carry(a, value);
            2
        }),
        dop!("RST 08H", &stack_rst!(0x08)),
        dop!("RET NC", &CPU::stack_ret_nc),
        dop!("POP DE", &|cpu: &mut CPU| {
            let value = cpu.stack_pop();
            cpu.regs.set_de(value);
            3
        }),
        dop!("JP NC,{:#06X}", u16, &CPU::jump_nc),
        dop!("0xD3 NOPE", &CPU::nonexistant),
        dop!("CALL NC,{:#06X}", u16, &CPU::stack_call_nc),
        dop!("PUSH DE", &CPU::stack_push_de),
        dop!("SUB A,{:#04X}", u8, &|cpu: &mut CPU, value| {
            let a = cpu.regs.a;
            cpu.regs.a = cpu.alu_sub_byte(a, value);
            2
        }),
        dop!("RST 10H", &stack_rst!(0x10)),
        dop!("RET C", &CPU::stack_ret_c),
        dop!("RETI", &CPU::stack_reti),
        dop!("JP C,{:#06X}", u16, &CPU::jump_c),
        dop!("0xDB NOPE", &CPU::nonexistant),
        dop!("CALL C,{:#06X}", u16, &CPU::stack_call_c),
        dop!("0xDD NOPE", &CPU::nonexistant),
        dop!("SBC A,{:#04X}", u8, &|cpu: &mut CPU, value| {
            let a = cpu.regs.a;
            cpu.regs.a = cpu.alu_sub_byte_with_carry(a, value);
            2
        }),
        dop!("RST 18H", &stack_rst!(0x18)),
        dop!("LD ({:#04X}),A", u8, &|cpu: &mut CPU, addr| {
            let value = cpu.regs.a;
            cpu.mmu.write_byte(addr as u16 + 0xFF00, value);
            3
        }),
        dop!("POP HL", &|cpu: &mut CPU| {
            let value = cpu.stack_pop();
            cpu.regs.set_hl(value);
            3
        }),
        dop!("LD (C),A", &|cpu: &mut CPU| {
            let addr = cpu.regs.c;
            let value = cpu.regs.a;
            cpu.mmu.write_byte(addr as u16 + 0xFF00, value);
            2
        }),
        dop!("0xE3 NOPE", &CPU::nonexistant),
        dop!("0xE4 NOPE", &CPU::nonexistant),
        dop!("PUSH HL", &CPU::stack_push_hl),
        dop!("AND A,{:#04X}", u8, &|cpu: &mut CPU, value| {
            let a = cpu.regs.a;
            cpu.regs.a = cpu.alu_and(a, value);
            2
        }),
        dop!("RST 20H", &stack_rst!(0x20)),
        dop!("ADD SP,{:#04X}", i8, &|cpu: &mut CPU, value| {
            cpu.regs.sp = cpu.alu_add_to_sp(value);
            4
        }),
        dop!("JP HL", &|cpu: &mut CPU| {
            cpu.regs.pc = cpu.regs.get_hl();
            1
        }),
        dop!("LD ({:#06X}),A", u16, &|cpu: &mut CPU, addr| {
            cpu.mmu.write_byte(addr, cpu.regs.a);
            4
        }),
        dop!("0xEB NOPE", &CPU::nonexistant),
        dop!("0xEC NOPE", &CPU::nonexistant),
        dop!("0xED NOPE", &CPU::nonexistant),
        dop!("XOR A,{:#04X}", u8, &|cpu: &mut CPU, value| {
            let a = cpu.regs.a;
            cpu.regs.a = cpu.alu_xor(a, value);
            2
        }),
        dop!("RST 28H", &stack_rst!(0x28)),
        dop!("LD A,({:#04X})", u8, &|cpu: &mut CPU, addr| {
            cpu.regs.a = cpu.mmu.read_byte(addr as u16 + 0xFF00);
            3
        }),
        dop!("POP AF", &|cpu: &mut CPU| {
            let value = cpu.stack_pop();
            cpu.regs.set_af(value & 0xFFF0); // make sure impossible flags are not set
            3
        }),
        dop!("LD A,(C)", &|cpu: &mut CPU| {
            let addr = cpu.regs.c;
            cpu.regs.a = cpu.mmu.read_byte(addr as u16 + 0xFF00);
            2
        }),
        dop!("DI", &CPU::disable_interupts),
        dop!("0xF4 NOPE", &CPU::nonexistant),
        dop!("PUSH AF", &CPU::stack_push_af),
        dop!("OR {:#04X}", u8, &|cpu: &mut CPU, value| {
            let a = cpu.regs.a;
            cpu.regs.a = cpu.alu_or(a, value);
            2
        }),
        dop!("RST 30H", &stack_rst!(0x30)),
        dop!("LD HL,SP+{:#04X}", i8, &|cpu: &mut CPU, value| {
            let r = cpu.alu_add_to_sp(value);
            cpu.regs.set_hl(r);
            3
        }),
        dop!("LD SP,HL", &|cpu: &mut CPU| {
            let hl = cpu.regs.get_hl();
            cpu.regs.sp = hl;
            2
        }),
        dop!("LD A,({:#06X})", u16, &|cpu: &mut CPU, addr| {
            cpu.regs.a = cpu.mmu.read_byte(addr);
            4
        }),
        dop!("EI", &CPU::enable_interupts),
        dop!("0xFC NOPE", &CPU::nonexistant),
        dop!("0xFD NOPE", &CPU::nonexistant),
        dop!("CP A,{:#04X}", u8, &|cpu: &mut CPU, value| {
            let a = cpu.regs.a;
            cpu.alu_cp(a, value);
            2
        }),
        dop!("RST 38H", &stack_rst!(0x38)),
    ];

    const CB_OPS: &'static [Operation] = &[
        dop!("RLC B", &rlc!(b)),
        dop!("RLC C", &rlc!(c)),
        dop!("RLC D", &rlc!(d)),
        dop!("RLC E", &rlc!(e)),
        dop!("RLC H", &rlc!(h)),
        dop!("RLC L", &rlc!(l)),
        dop!("RLC (HL)", &rlc_hl!()),
        dop!("RLC A", &rlc!(a)),
        dop!("RRC B", &rrc!(b)),
        dop!("RRC C", &rrc!(c)),
        dop!("RRC D", &rrc!(d)),
        dop!("RRC E", &rrc!(e)),
        dop!("RRC H", &rrc!(h)),
        dop!("RRC L", &rrc!(l)),
        dop!("RRC (HL)", &rrc_hl!()),
        dop!("RRC A", &rrc!(a)),
        dop!("RL B", &rl!(b)),
        dop!("RL C", &rl!(c)),
        dop!("RL D", &rl!(d)),
        dop!("RL E", &rl!(e)),
        dop!("RL H", &rl!(h)),
        dop!("RL L", &rl!(l)),
        dop!("RL (HL)", &rl_hl!()),
        dop!("RL A", &rl!(a)),
        dop!("RR B", &rr!(b)),
        dop!("RR C", &rr!(c)),
        dop!("RR D", &rr!(d)),
        dop!("RR E", &rr!(e)),
        dop!("RR H", &rr!(h)),
        dop!("RR L", &rr!(l)),
        dop!("RR (HL)", &rr_hl!()),
        dop!("RR A", &rr!(a)),
        dop!("SLA B", &sla!(b)),
        dop!("SLA C", &sla!(c)),
        dop!("SLA D", &sla!(d)),
        dop!("SLA E", &sla!(e)),
        dop!("SLA H", &sla!(h)),
        dop!("SLA L", &sla!(l)),
        dop!("SLA (HL)", &sla_hl!()),
        dop!("SLA A", &sla!(a)),
        dop!("SRA B", &sra!(b)),
        dop!("SRA C", &sra!(c)),
        dop!("SRA D", &sra!(d)),
        dop!("SRA E", &sra!(e)),
        dop!("SRA H", &sra!(h)),
        dop!("SRA L", &sra!(l)),
        dop!("SRA (HL)", &sra_hl!()),
        dop!("SRA A", &sra!(a)),
        dop!("SWAP B", &swap!(b)),
        dop!("SWAP C", &swap!(c)),
        dop!("SWAP D", &swap!(d)),
        dop!("SWAP E", &swap!(e)),
        dop!("SWAP H", &swap!(h)),
        dop!("SWAP L", &swap!(l)),
        dop!("SWAP (HL)", &swap_hl!()),
        dop!("SWAP A", &swap!(a)),
        dop!("SRL B", &srl!(b)),
        dop!("SRL C", &srl!(c)),
        dop!("SRL D", &srl!(d)),
        dop!("SRL E", &srl!(e)),
        dop!("SRL H", &srl!(h)),
        dop!("SRL L", &srl!(l)),
        dop!("SRL (HL)", &srl_hl!()),
        dop!("SRL A", &srl!(a)),
        dop!("BIT 0,B", &bit!(0, b)),
        dop!("BIT 0,C", &bit!(0, c)),
        dop!("BIT 0,D", &bit!(0, d)),
        dop!("BIT 0,E", &bit!(0, e)),
        dop!("BIT 0,H", &bit!(0, h)),
        dop!("BIT 0,L", &bit!(0, l)),
        dop!("BIT 0,(HL)", &bit_hl!(0)),
        dop!("BIT 0,A", &bit!(0, a)),
        dop!("BIT 1,B", &bit!(1, b)),
        dop!("BIT 1,C", &bit!(1, c)),
        dop!("BIT 1,D", &bit!(1, d)),
        dop!("BIT 1,E", &bit!(1, e)),
        dop!("BIT 1,H", &bit!(1, h)),
        dop!("BIT 1,L", &bit!(1, l)),
        dop!("BIT 1,(HL)", &bit_hl!(1)),
        dop!("BIT 1,A", &bit!(1, a)),
        dop!("BIT 2,B", &bit!(2, b)),
        dop!("BIT 2,C", &bit!(2, c)),
        dop!("BIT 2,D", &bit!(2, d)),
        dop!("BIT 2,E", &bit!(2, e)),
        dop!("BIT 2,H", &bit!(2, h)),
        dop!("BIT 2,L", &bit!(2, l)),
        dop!("BIT 2,(HL)", &bit_hl!(2)),
        dop!("BIT 2,A", &bit!(2, a)),
        dop!("BIT 3,B", &bit!(3, b)),
        dop!("BIT 3,C", &bit!(3, c)),
        dop!("BIT 3,D", &bit!(3, d)),
        dop!("BIT 3,E", &bit!(3, e)),
        dop!("BIT 3,H", &bit!(3, h)),
        dop!("BIT 3,L", &bit!(3, l)),
        dop!("BIT 3,(HL)", &bit_hl!(3)),
        dop!("BIT 3,A", &bit!(3, a)),
        dop!("BIT 4,B", &bit!(4, b)),
        dop!("BIT 4,C", &bit!(4, c)),
        dop!("BIT 4,D", &bit!(4, d)),
        dop!("BIT 4,E", &bit!(4, e)),
        dop!("BIT 4,H", &bit!(4, h)),
        dop!("BIT 4,L", &bit!(4, l)),
        dop!("BIT 4,(HL)", &bit_hl!(4)),
        dop!("BIT 4,A", &bit!(4, a)),
        dop!("BIT 5,B", &bit!(5, b)),
        dop!("BIT 5,C", &bit!(5, c)),
        dop!("BIT 5,D", &bit!(5, d)),
        dop!("BIT 5,E", &bit!(5, e)),
        dop!("BIT 5,H", &bit!(5, h)),
        dop!("BIT 5,L", &bit!(5, l)),
        dop!("BIT 5,(HL)", &bit_hl!(5)),
        dop!("BIT 5,A", &bit!(5, a)),
        dop!("BIT 6,B", &bit!(6, b)),
        dop!("BIT 6,C", &bit!(6, c)),
        dop!("BIT 6,D", &bit!(6, d)),
        dop!("BIT 6,E", &bit!(6, e)),
        dop!("BIT 6,H", &bit!(6, h)),
        dop!("BIT 6,L", &bit!(6, l)),
        dop!("BIT 6,(HL)", &bit_hl!(6)),
        dop!("BIT 6,A", &bit!(6, a)),
        dop!("BIT 7,B", &bit!(7, b)),
        dop!("BIT 7,C", &bit!(7, c)),
        dop!("BIT 7,D", &bit!(7, d)),
        dop!("BIT 7,E", &bit!(7, e)),
        dop!("BIT 7,H", &bit!(7, h)),
        dop!("BIT 7,L", &bit!(7, l)),
        dop!("BIT 7,(HL)", &bit_hl!(7)),
        dop!("BIT 7,A", &bit!(7, a)),
        dop!("RES 0,B", &res!(0, b)),
        dop!("RES 0,C", &res!(0, c)),
        dop!("RES 0,D", &res!(0, d)),
        dop!("RES 0,E", &res!(0, e)),
        dop!("RES 0,H", &res!(0, h)),
        dop!("RES 0,L", &res!(0, l)),
        dop!("RES 0,(HL)", &res_hl!(0)),
        dop!("RES 0,A", &res!(0, a)),
        dop!("RES 1,B", &res!(1, b)),
        dop!("RES 1,C", &res!(1, c)),
        dop!("RES 1,D", &res!(1, d)),
        dop!("RES 1,E", &res!(1, e)),
        dop!("RES 1,H", &res!(1, h)),
        dop!("RES 1,L", &res!(1, l)),
        dop!("RES 1,(HL)", &res_hl!(1)),
        dop!("RES 1,A", &res!(1, a)),
        dop!("RES 2,B", &res!(2, b)),
        dop!("RES 2,C", &res!(2, c)),
        dop!("RES 2,D", &res!(2, d)),
        dop!("RES 2,E", &res!(2, e)),
        dop!("RES 2,H", &res!(2, h)),
        dop!("RES 2,L", &res!(2, l)),
        dop!("RES 2,(HL)", &res_hl!(2)),
        dop!("RES 2,A", &res!(2, a)),
        dop!("RES 3,B", &res!(3, b)),
        dop!("RES 3,C", &res!(3, c)),
        dop!("RES 3,D", &res!(3, d)),
        dop!("RES 3,E", &res!(3, e)),
        dop!("RES 3,H", &res!(3, h)),
        dop!("RES 3,L", &res!(3, l)),
        dop!("RES 3,(HL)", &res_hl!(3)),
        dop!("RES 3,A", &res!(3, a)),
        dop!("RES 4,B", &res!(4, b)),
        dop!("RES 4,C", &res!(4, c)),
        dop!("RES 4,D", &res!(4, d)),
        dop!("RES 4,E", &res!(4, e)),
        dop!("RES 4,H", &res!(4, h)),
        dop!("RES 4,L", &res!(4, l)),
        dop!("RES 4,(HL)", &res_hl!(4)),
        dop!("RES 4,A", &res!(4, a)),
        dop!("RES 5,B", &res!(5, b)),
        dop!("RES 5,C", &res!(5, c)),
        dop!("RES 5,D", &res!(5, d)),
        dop!("RES 5,E", &res!(5, e)),
        dop!("RES 5,H", &res!(5, h)),
        dop!("RES 5,L", &res!(5, l)),
        dop!("RES 5,(HL)", &res_hl!(5)),
        dop!("RES 5,A", &res!(5, a)),
        dop!("RES 6,B", &res!(6, b)),
        dop!("RES 6,C", &res!(6, c)),
        dop!("RES 6,D", &res!(6, d)),
        dop!("RES 6,E", &res!(6, e)),
        dop!("RES 6,H", &res!(6, h)),
        dop!("RES 6,L", &res!(6, l)),
        dop!("RES 6,(HL)", &res_hl!(6)),
        dop!("RES 6,A", &res!(6, a)),
        dop!("RES 7,B", &res!(7, b)),
        dop!("RES 7,C", &res!(7, c)),
        dop!("RES 7,D", &res!(7, d)),
        dop!("RES 7,E", &res!(7, e)),
        dop!("RES 7,H", &res!(7, h)),
        dop!("RES 7,L", &res!(7, l)),
        dop!("RES 7,(HL)", &res_hl!(7)),
        dop!("RES 7,A", &res!(7, a)),
        dop!("SET 0,B", &set!(0, b)),
        dop!("SET 0,C", &set!(0, c)),
        dop!("SET 0,D", &set!(0, d)),
        dop!("SET 0,E", &set!(0, e)),
        dop!("SET 0,H", &set!(0, h)),
        dop!("SET 0,L", &set!(0, l)),
        dop!("SET 0,(HL)", &set_hl!(0)),
        dop!("SET 0,A", &set!(0, a)),
        dop!("SET 1,B", &set!(1, b)),
        dop!("SET 1,C", &set!(1, c)),
        dop!("SET 1,D", &set!(1, d)),
        dop!("SET 1,E", &set!(1, e)),
        dop!("SET 1,H", &set!(1, h)),
        dop!("SET 1,L", &set!(1, l)),
        dop!("SET 1,(HL)", &set_hl!(1)),
        dop!("SET 1,A", &set!(1, a)),
        dop!("SET 2,B", &set!(2, b)),
        dop!("SET 2,C", &set!(2, c)),
        dop!("SET 2,D", &set!(2, d)),
        dop!("SET 2,E", &set!(2, e)),
        dop!("SET 2,H", &set!(2, h)),
        dop!("SET 2,L", &set!(2, l)),
        dop!("SET 2,(HL)", &set_hl!(2)),
        dop!("SET 2,A", &set!(2, a)),
        dop!("SET 3,B", &set!(3, b)),
        dop!("SET 3,C", &set!(3, c)),
        dop!("SET 3,D", &set!(3, d)),
        dop!("SET 3,E", &set!(3, e)),
        dop!("SET 3,H", &set!(3, h)),
        dop!("SET 3,L", &set!(3, l)),
        dop!("SET 3,(HL)", &set_hl!(3)),
        dop!("SET 3,A", &set!(3, a)),
        dop!("SET 4,B", &set!(4, b)),
        dop!("SET 4,C", &set!(4, c)),
        dop!("SET 4,D", &set!(4, d)),
        dop!("SET 4,E", &set!(4, e)),
        dop!("SET 4,H", &set!(4, h)),
        dop!("SET 4,L", &set!(4, l)),
        dop!("SET 4,(HL)", &set_hl!(4)),
        dop!("SET 4,A", &set!(4, a)),
        dop!("SET 5,B", &set!(5, b)),
        dop!("SET 5,C", &set!(5, c)),
        dop!("SET 5,D", &set!(5, d)),
        dop!("SET 5,E", &set!(5, e)),
        dop!("SET 5,H", &set!(5, h)),
        dop!("SET 5,L", &set!(5, l)),
        dop!("SET 5,(HL)", &set_hl!(5)),
        dop!("SET 5,A", &set!(5, a)),
        dop!("SET 6,B", &set!(6, b)),
        dop!("SET 6,C", &set!(6, c)),
        dop!("SET 6,D", &set!(6, d)),
        dop!("SET 6,E", &set!(6, e)),
        dop!("SET 6,H", &set!(6, h)),
        dop!("SET 6,L", &set!(6, l)),
        dop!("SET 6,(HL)", &set_hl!(6)),
        dop!("SET 6,A", &set!(6, a)),
        dop!("SET 7,B", &set!(7, b)),
        dop!("SET 7,C", &set!(7, c)),
        dop!("SET 7,D", &set!(7, d)),
        dop!("SET 7,E", &set!(7, e)),
        dop!("SET 7,H", &set!(7, h)),
        dop!("SET 7,L", &set!(7, l)),
        dop!("SET 7,(HL)", &set_hl!(7)),
        dop!("SET 7,A", &set!(7, a)),
    ];

    pub fn new(rom_data: &Vec<u8>) -> CPU {
        // print!("OPCODES");
        // for (addr, op) in Self::OPS.iter().enumerate() {
        //     println!("{:02x} {:}", addr, op.mneumonic);
        // }

        // print!("CB OPCODES");
        // for (addr, op) in Self::CB_OPS.iter().enumerate() {
        //     println!("{:02x} {:}", addr, op.mneumonic);
        // }

        CPU {
            regs: Registers::new(),
            mmu: MMU::new(rom_data),
            halted: false,
        }
    }

    pub fn cycle(&mut self) -> usize {
        self.process()
    }

    fn process(&mut self) -> usize {
        let regs = self.regs;
        let op_code = self.fetch_byte();
        let op = &CPU::OPS[op_code as usize];
        if op.size == 3 {
            let arg = self.mmu.read_word(self.regs.pc);
            println!(
                "OP: {:#04X} {:20} {:#06X} {:}",
                op_code,
                op.mneumonic,
                arg,
                regs.one_line_rep()
            );
        } else if op.size == 2 {
            let arg = self.mmu.read_byte(self.regs.pc);
            println!(
                "OP: {:#04X} {:20}   {:#04X} {:}",
                op_code,
                op.mneumonic,
                arg,
                regs.one_line_rep()
            );
        } else {
            println!(
                "OP: {:#04X} {:20}        {:}",
                op_code,
                op.mneumonic,
                regs.one_line_rep()
            );
        }
        let op_impl = op.execute;

        let ticks = op_impl(self);

        return ticks;
    }

    fn process_cb(&mut self) -> usize {
        let op_code = self.fetch_byte();
        let op = &CPU::CB_OPS[op_code as usize];

        if op.size == 3 {
            let arg = self.mmu.read_word(self.regs.pc);
            println!("CB OP: {:#04X} {:20} {:#06X}", op_code, op.mneumonic, arg);
        } else if op.size == 2 {
            let arg = self.mmu.read_byte(self.regs.pc);
            println!("CB OP: {:#04X} {:20}   {:#04X}", op_code, op.mneumonic, arg);
        } else {
            println!("CB OP: {:#04X} {:20}", op_code, op.mneumonic);
        }

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
        println!("enable_interupts");
        self.regs.ime = true;
        // TODO: enable interupts only an instruction later
        1
    }

    fn disable_interupts(&mut self) -> usize {
        println!("disable_interupts");
        self.regs.ime = false;
        // TODO: disable interupts only an instruction later
        1
    }

    fn stop(&mut self) -> usize {
        println!("STOP not implemented");
        1
    }

    fn halt(&mut self) -> usize {
        println!("HALT not implemented");
        1
    }

    fn nonexistant(&mut self) -> usize {
        unimplemented!("this op doesn't exist on gb hardware")
    }

    fn unimplemented(&mut self) -> usize {
        unimplemented!("op is unimplemented")
    }

    // DEBUG

    pub fn print_reg_state(&self) {
        println!("REGS: {:?}", self.regs);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_cpu_with_single_instruction(instruction: Vec<u8>) -> CPU {
        let mut rom_data = vec![0x00; 0x10000];
        rom_data[0x100..(0x100 + instruction.len())].copy_from_slice(&instruction);
        CPU::new(&rom_data)
    }

    #[test]
    fn test_cpu_instr_nop() {
        let mut cpu = create_cpu_with_single_instruction(vec![0x00]);
        let mut old_regs = cpu.regs;
        cpu.cycle();
        old_regs.pc += 1;
        assert_eq!(cpu.regs, old_regs);
    }

    #[test]
    fn test_cpu_instr_ld() {
        let mut cpu = create_cpu_with_single_instruction(vec![0x01, 0xde, 0xad]);
        let mut old_regs = cpu.regs;
        cpu.cycle();
        old_regs.b = 0xad;
        old_regs.c = 0xde;
        old_regs.pc += 3;
        assert_eq!(cpu.regs, old_regs);
    }
}
