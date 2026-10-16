use super::*;

const VBLANK: u8 = 1 << 0;

/// A DMG with `program` at 0x100, the LCD off and no interrupts pending.
fn cpu(program: &[u8]) -> Cpu {
    let mut rom = vec![0x00; 0x8000];
    rom[0x100..0x100 + program.len()].copy_from_slice(program);
    let mut cpu = Cpu::new(Cartridge::new(rom, None), Model::Dmg);
    cpu.mmu.write_byte(0xFF40, 0x00);
    cpu.mmu.write_byte(0xFF0F, 0x00);
    cpu.regs.sp = 0xDFF0;
    cpu
}

fn request_vblank(cpu: &mut Cpu) {
    cpu.mmu.write_byte(0xFFFF, VBLANK);
    cpu.mmu.write_byte(0xFF0F, VBLANK);
}

fn pushed_return_address(cpu: &Cpu) -> u16 {
    cpu.mmu.read_word(cpu.regs.sp)
}

#[test]
fn nop_only_advances_pc() {
    let mut cpu = cpu(&[0x00]);
    let mut expected = cpu.regs;
    assert_eq!(cpu.cycle(), 1);
    expected.pc += 1;
    assert_eq!(cpu.regs, expected);
}

#[test]
fn ld_rr_d16_loads_little_endian() {
    let mut cpu = cpu(&[0x01, 0xDE, 0xAD]);
    cpu.cycle();
    assert_eq!((cpu.regs.get_bc(), cpu.regs.pc), (0xADDE, 0x103));
}

#[test]
fn instructions_take_their_documented_m_cycles() {
    for (program, cycles) in [
        (&[0x00][..], 1),
        (&[0x01, 0, 0], 3),
        (&[0x18, 0x00], 3),
        (&[0x20, 0x00], 2),
        (&[0x28, 0x00], 3),
        (&[0xC3, 0x00, 0x02], 4),
        (&[0xCD, 0x00, 0x02], 6),
        (&[0xC9], 4),
        (&[0xC5], 4),
        (&[0xC1], 3),
        (&[0x36, 0x00], 3),
        (&[0x34], 3),
        (&[0xCB, 0x7E], 3),
        (&[0xCB, 0xC6], 4),
        (&[0xE8, 0x01], 4),
        (&[0xF8, 0x01], 3),
    ] {
        let mut cpu = cpu(program);
        cpu.regs.set_hl(0xC000);
        assert_eq!(cpu.cycle(), cycles, "{program:02X?}");
    }
}

#[test]
fn pop_af_clears_the_low_flag_bits() {
    let mut cpu = cpu(&[0xF1]);
    cpu.mmu.write_byte(0xDFF0, 0xFF);
    cpu.mmu.write_byte(0xDFF1, 0x12);
    cpu.cycle();
    assert_eq!(cpu.regs.get_af(), 0x12F0);
}

#[test]
fn an_interrupt_pushes_pc_and_jumps_to_its_vector() {
    let mut cpu = cpu(&[0x00]);
    cpu.regs.ime = true;
    request_vblank(&mut cpu);
    assert_eq!(cpu.cycle(), 5);
    assert_eq!(cpu.regs.pc, 0x40);
    assert_eq!(pushed_return_address(&cpu), 0x100);
    assert!(!cpu.regs.ime);
    assert_eq!(cpu.mmu.pending_interrupts(), 0);
}

#[test]
fn the_lowest_numbered_pending_interrupt_wins() {
    let mut cpu = cpu(&[0x00]);
    cpu.regs.ime = true;
    cpu.mmu.write_byte(0xFFFF, 0x1F);
    cpu.mmu.write_byte(0xFF0F, 0b10100);
    cpu.cycle();
    assert_eq!(cpu.regs.pc, 0x50);
    assert_eq!(cpu.mmu.read_byte(0xFF0F) & 0x1F, 0b10000);
}

#[test]
fn ei_takes_effect_after_the_next_instruction() {
    let mut cpu = cpu(&[0xFB, 0x00, 0x00]);
    request_vblank(&mut cpu);
    cpu.cycle();
    cpu.cycle();
    assert_eq!(cpu.regs.pc, 0x102, "the instruction after EI runs");
    cpu.cycle();
    assert_eq!((cpu.regs.pc, pushed_return_address(&cpu)), (0x40, 0x102));
}

#[test]
fn di_straight_after_ei_keeps_interrupts_off() {
    let mut cpu = cpu(&[0xFB, 0xF3, 0x00, 0x00]);
    request_vblank(&mut cpu);
    for _ in 0..4 {
        cpu.cycle();
    }
    assert_eq!(cpu.regs.pc, 0x104);
    assert!(!cpu.regs.ime);
}

#[test]
fn reti_enables_interrupts_at_once() {
    let mut cpu = cpu(&[0xD9]);
    cpu.mmu.write_byte(0xDFF0, 0x34);
    cpu.mmu.write_byte(0xDFF1, 0x12);
    cpu.cycle();
    assert_eq!((cpu.regs.pc, cpu.regs.ime), (0x1234, true));
}

#[test]
fn halt_without_interrupts_enabled_wakes_and_continues() {
    let mut cpu = cpu(&[0x76, 0x3C]);
    cpu.mmu.write_byte(0xFFFF, VBLANK);
    cpu.cycle();
    assert!(cpu.halted);
    cpu.cycle();
    assert!(cpu.halted, "nothing pending yet");
    cpu.mmu.write_byte(0xFF0F, VBLANK);
    cpu.cycle();
    assert!(!cpu.halted);
    assert_eq!(
        cpu.regs.pc, 0x102,
        "the next instruction ran without a dispatch"
    );
}

#[test]
fn halt_with_a_pending_interrupt_and_ime_off_runs_the_next_byte_twice() {
    let mut cpu = cpu(&[0x76, 0x3C, 0x00]);
    request_vblank(&mut cpu);
    let a = cpu.regs.a;
    for _ in 0..3 {
        cpu.cycle();
    }
    assert_eq!(cpu.regs.a, a.wrapping_add(2));
    assert_eq!(cpu.regs.pc, 0x102);
    assert!(!cpu.halted);
}

#[test]
fn halt_with_a_pending_interrupt_and_ime_on_returns_to_the_halt() {
    let mut cpu = cpu(&[0xFB, 0x76]);
    request_vblank(&mut cpu);
    cpu.cycle();
    cpu.cycle();
    assert!(!cpu.halted);
    cpu.cycle();
    assert_eq!((cpu.regs.pc, pushed_return_address(&cpu)), (0x40, 0x101));
}

#[test]
fn a_halted_cpu_dispatches_when_an_interrupt_arrives() {
    let mut cpu = cpu(&[0x76, 0x00]);
    cpu.regs.ime = true;
    cpu.mmu.write_byte(0xFFFF, VBLANK);
    cpu.cycle();
    cpu.cycle();
    cpu.mmu.write_byte(0xFF0F, VBLANK);
    cpu.cycle();
    assert_eq!((cpu.regs.pc, pushed_return_address(&cpu)), (0x40, 0x101));
}

#[test]
fn invalid_opcodes_lock_the_cpu() {
    let mut cpu = cpu(&[0xD3, 0x00]);
    cpu.regs.ime = true;
    cpu.cycle();
    request_vblank(&mut cpu);
    for _ in 0..3 {
        cpu.cycle();
    }
    assert!(cpu.locked);
    assert_eq!(cpu.regs.pc, 0x101);
}

#[test]
fn hl_increments_and_decrements_wrap() {
    let mut cpu = cpu(&[0x32, 0x22, 0x3A, 0x2A]);
    cpu.regs.set_hl(0x0000);
    cpu.cycle();
    assert_eq!(cpu.regs.get_hl(), 0xFFFF);
    cpu.cycle();
    assert_eq!(cpu.regs.get_hl(), 0x0000);
    cpu.cycle();
    assert_eq!(cpu.regs.get_hl(), 0xFFFF);
    cpu.cycle();
    assert_eq!(cpu.regs.get_hl(), 0x0000);
}
