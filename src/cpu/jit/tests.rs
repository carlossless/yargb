use super::*;
use crate::cartridge::Cartridge;
use crate::mmu::Model;

fn assert_lockstep(
    rom: &[u8],
    model: Model,
    hot_threshold: u16,
    steps: usize,
    stop_at: Option<u16>,
) {
    let rom = rom.to_vec();
    let mut interpreted = Cpu::new(Cartridge::new(rom.clone(), None), model);
    let mut jitted = Cpu::new(Cartridge::new(rom.clone(), None), model);
    let mut jit = Jit::new(&jitted);
    jit.hot_threshold = hot_threshold;
    let state = |cpu: &Cpu| {
        (
            cpu.regs,
            cpu.halted,
            cpu.halt_bug,
            cpu.stopped,
            cpu.ime_toggle,
        )
    };
    for step in 0..steps {
        if stop_at == Some(jitted.regs.pc) {
            return;
        }
        let start = jitted.regs;
        let start_ppu = format!(
            "{} IE {:02X} IF {:02X} budget {}",
            jitted.mmu.ppu.describe(),
            jitted.mmu.read_byte(0xFFFF),
            jitted.mmu.read_byte(0xFF0F),
            jitted.mmu.m_cycles_until_event(true)
        );
        let result = jit.step(&mut jitted);
        let ticks: usize = (0..result.instructions).map(|_| interpreted.cycle()).sum();
        let memory_differs = interpreted.mmu.apu.take_samples() != jitted.mmu.apu.take_samples()
            || !interpreted.mmu.same_state(&jitted.mmu);
        if ticks != result.ticks || state(&interpreted) != state(&jitted) || memory_differs {
            panic!(
                "diverged at step {step} after {} instructions from {start:?}\n\
                 ticks: interpreter {ticks}, jit {}, memory differs: {memory_differs}\n\
                 interpreter: {:?}\n\
                 jit:         {:?}\n\
                 hardware at start: {start_ppu}\n\
                 hardware now: {}",
                result.instructions,
                result.ticks,
                state(&interpreted),
                state(&jitted),
                jitted.mmu.ppu.describe(),
            );
        }
    }
}

#[test]
fn matches_interpreter_on_cpu_instrs() {
    let rom = std::fs::read(crate::test_roms::test_rom(
        "blargg/cpu_instrs/cpu_instrs.gb",
    ))
    .unwrap();
    assert_lockstep(&rom, Model::Dmg, 16, 20_000_000, None);
}

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }

    fn below(&mut self, n: usize) -> usize {
        self.next() as usize % n
    }

    fn byte(&mut self) -> u8 {
        self.next() as u8
    }

    fn pick<T: Copy>(&mut self, items: &[T]) -> T {
        items[self.below(items.len())]
    }

    fn wram(&mut self) -> [u8; 2] {
        let addr = 0xC000 + self.below(0x0800) as u16;
        addr.to_le_bytes()
    }
}

const SUBROUTINE: u16 = 0x2000;
const RAM_ROUTINE: u16 = 0xC800;
const CODE_START: u16 = 0x150;

fn register_op(rng: &mut Rng, moves_sp: bool) -> Vec<u8> {
    let reg = || [0u8, 1, 2, 3, 4, 5, 7];
    match rng.below(if moves_sp { 12 } else { 10 }) {
        0 => vec![0x40 | rng.pick(&reg()) << 3 | rng.pick(&reg())],
        1 => vec![0x80 | (rng.below(8) as u8) << 3 | rng.pick(&reg())],
        2 => vec![0xC6 | (rng.below(8) as u8) << 3, rng.byte()],
        3 => vec![0x04 | rng.pick(&reg()) << 3 | rng.below(2) as u8],
        4 => vec![rng.pick(&[0x03, 0x13, 0x23, 0x0B, 0x1B, 0x2B, 0x09, 0x19, 0x29])],
        5 => vec![rng.pick(&[0x07, 0x0F, 0x17, 0x1F, 0x27, 0x2F, 0x37, 0x3F, 0x00])],
        6 => vec![0xCB, rng.byte() & 0xF8 | rng.pick(&reg())],
        7 => vec![0x06 | rng.pick(&reg()) << 3, rng.byte()],
        8 => vec![0xF5, rng.pick(&[0xC1, 0xD1, 0xE1, 0xF1])],
        9 => vec![rng.pick(&[0xC5, 0xD5, 0xE5]), rng.pick(&[0xC1, 0xD1, 0xE1])],
        10 => vec![0xF8, rng.byte(), 0xF9, 0x31, 0x00, 0xD0],
        _ => vec![0xE8, rng.byte(), 0x31, 0x00, 0xD0],
    }
}

fn memory_op(rng: &mut Rng) -> Vec<u8> {
    let [lo, hi] = rng.wram();
    let mut code = vec![0x21, lo, hi];
    match rng.below(15) {
        10 => {
            let [lo, hi] = (0x8000 + rng.below(0x2000) as u16).to_le_bytes();
            code = vec![0x21, lo, hi, rng.pick(&[0x77, 0x7E, 0x34])];
        }
        11 => {
            let op = rng.pick(&[0x77, 0x7E, 0x23, 0x2B, 0x22, 0x32, 0x2A, 0x3A]);
            code = vec![0x21, rng.byte(), 0xFE, op];
        }
        12 => code = vec![0xF0, rng.pick(&[0x41, 0x44, 0x26, 0x12, 0x30, 0x00])],
        13 => {
            let port = rng.pick(&[
                0x10, 0x11, 0x12, 0x13, 0x14, 0x16, 0x17, 0x19, 0x1A, 0x1C, 0x1E, 0x21, 0x22, 0x23,
                0x24, 0x25, 0x26, 0x30, 0x40, 0x41, 0x42, 0x43, 0x45, 0x47, 0x4A, 0x4B,
            ]);
            let value = match port {
                0x40 => rng.pick(&[0x91, 0x93, 0xB3, 0xE3, 0x11]),
                0x45 => rng.below(154) as u8,
                _ => rng.byte(),
            };
            code = vec![0x3E, value, 0xE0, port];
        }
        14 => code = vec![0x3E, 0xC0 + rng.below(8) as u8, 0xE0, 0x46],
        0 => code.push(0x46 | (rng.pick(&[0u8, 1, 2, 3, 7])) << 3),
        1 => code.push(0x70 | rng.pick(&[0u8, 1, 2, 3, 4, 5, 7])),
        2 => code.push(0x86 | (rng.below(8) as u8) << 3),
        3 => code.push(rng.pick(&[0x34, 0x35])),
        4 => code.extend([0x36, rng.byte()]),
        5 => code.push(rng.pick(&[0x22, 0x32, 0x2A, 0x3A])),
        6 => code.extend([0xCB, rng.byte() & 0xF8 | 6]),
        7 => {
            let pair = rng.pick(&[0x00, 0x10]);
            code.extend([0x01 | pair, lo, hi, rng.pick(&[0x02, 0x0A]) | pair]);
        }
        8 => code.extend([rng.pick(&[0xEA, 0xFA]), lo, hi]),
        _ => {
            let port = rng.pick(&[
                0x80, 0x9F, 0xC0, 0xFE, 0x04, 0x05, 0x06, 0x07, 0x01, 0x02, 0x0F,
            ]);
            code.extend([rng.pick(&[0xE0, 0xF0]), port]);
        }
    }
    code
}

fn control_op(rng: &mut Rng, at: u16) -> Vec<u8> {
    let [sub_lo, sub_hi] = SUBROUTINE.to_le_bytes();
    let [ram_lo, ram_hi] = RAM_ROUTINE.to_le_bytes();
    match rng.below(9) {
        0 => vec![rng.pick(&[0x20, 0x28, 0x30, 0x38, 0x18]), 0x01, 0x00],
        1 => {
            let [lo, hi] = (at + 4).to_le_bytes();
            vec![rng.pick(&[0xC2, 0xCA, 0xD2, 0xDA, 0xC3]), lo, hi, 0x00]
        }
        2 => vec![rng.pick(&[0xC4, 0xCC, 0xD4, 0xDC, 0xCD]), sub_lo, sub_hi],
        3 => vec![0xFF],
        4 => vec![rng.pick(&[0xF3, 0xFB, 0x76]), 0x00],
        5 => vec![0xCD, ram_lo, ram_hi],
        6 => vec![
            0x3E,
            rng.pick(&[0x04, 0x0C, 0x15, 0x3C, 0xAF]),
            0xEA,
            ram_lo,
            ram_hi,
        ],
        7 => {
            let [lo, hi] = (at + 5).to_le_bytes();
            vec![0x21, lo, hi, 0xE9, 0x00]
        }
        _ => vec![0xCD, ram_lo, ram_hi, 0xCD, ram_lo, ram_hi],
    }
}

const BANKED_ROUTINE: u16 = 0xD800;

fn cgb_op(rng: &mut Rng) -> Vec<u8> {
    let [lo, hi] = BANKED_ROUTINE.to_le_bytes();
    match rng.below(8) {
        0 => vec![0x3E, rng.below(8) as u8, 0xE0, 0x70],
        1 => vec![0x3E, rng.below(2) as u8, 0xE0, 0x4F],
        2 => {
            let (index_port, data_port) = rng.pick(&[(0x68, 0x69), (0x6A, 0x6B)]);
            vec![
                0x3E,
                rng.byte() | 0x80,
                0xE0,
                index_port,
                0x3E,
                rng.byte(),
                0xE0,
                data_port,
            ]
        }
        3 => vec![0xCD, lo, hi],
        4 => vec![0x3E, rng.pick(&[0x04, 0x14, 0x1D, 0xAF]), 0xEA, lo, hi],
        5 => {
            let mode = rng.pick(&[0x00, 0x80]);
            vec![
                0x3E,
                0xC0 + rng.below(8) as u8,
                0xE0,
                0x51,
                0x3E,
                rng.byte() & 0xF0,
                0xE0,
                0x52,
                0x3E,
                rng.below(0x20) as u8,
                0xE0,
                0x53,
                0x3E,
                rng.byte() & 0xF0,
                0xE0,
                0x54,
                0x3E,
                mode | rng.below(4) as u8,
                0xE0,
                0x55,
            ]
        }
        6 => vec![0x3E, 0x01, 0xE0, 0x4D, 0x10, 0x00],
        _ => vec![0x3E, rng.below(2) as u8, 0xE0, 0x6C],
    }
}

fn random_rom(seed: u64, cgb: bool) -> (Vec<u8>, u16) {
    let mut rng = Rng(seed.wrapping_mul(0x9E37_79B9_7F4A_7C15) | 1);
    let mut rom = vec![0u8; 0x8000];
    rom[0x38] = 0xC9;
    rom[0x40..0x42].copy_from_slice(&[0x04, 0xD9]);
    rom[0x48..0x4A].copy_from_slice(&[0x0C, 0xD9]);
    rom[0x50..0x52].copy_from_slice(&[0x1C, 0xD9]);

    let mut sub = Vec::new();
    for _ in 0..rng.below(6) {
        sub.extend(register_op(&mut rng, false));
    }
    sub.extend([0xD8, 0x3C, 0xC9]);
    rom[SUBROUTINE as usize..SUBROUTINE as usize + sub.len()].copy_from_slice(&sub);

    let [ram_lo, ram_hi] = RAM_ROUTINE.to_le_bytes();
    let mut code = vec![
        0x31,
        0x00,
        0xD0,
        0x3E,
        0x05,
        0xE0,
        0x07,
        0x3E,
        0x07,
        0xE0,
        0xFF,
        0x3E,
        rng.byte() & 0x78,
        0xE0,
        0x41,
    ];
    for (i, byte) in [0x04, 0x0C, 0xC9].into_iter().enumerate() {
        code.extend([0x3E, byte, 0xEA, ram_lo + i as u8, ram_hi]);
    }
    if cgb {
        rom[0x143] = 0x80;
        let [lo, hi] = BANKED_ROUTINE.to_le_bytes();
        for bank in 1..=7u8 {
            code.extend([0x3E, bank, 0xE0, 0x70]);
            let first = [0x04, 0x0C, 0x14, 0x1C, 0x05, 0x0D, 0x15][bank as usize - 1];
            for (i, byte) in [first, 0x2C, 0xC9].into_iter().enumerate() {
                code.extend([0x3E, byte, 0xEA, lo + i as u8, hi]);
            }
        }
    }
    code.push(0xFB);
    while code.len() < 0x1E00 - CODE_START as usize {
        let at = CODE_START + code.len() as u16;
        code.extend(match rng.below(if cgb { 4 } else { 3 }) {
            0 => register_op(&mut rng, true),
            1 => memory_op(&mut rng),
            2 => control_op(&mut rng, at),
            _ => cgb_op(&mut rng),
        });
    }
    let end = CODE_START + code.len() as u16;
    code.extend([0x18, 0xFE]);
    rom[0x100..0x104].copy_from_slice(&[0x00, 0xC3, 0x50, 0x01]);
    rom[CODE_START as usize..CODE_START as usize + code.len()].copy_from_slice(&code);
    (rom, end)
}

#[test]
fn matches_interpreter_on_random_programs() {
    let seeds = std::env::var("YARGB_FUZZ_SEEDS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(40);
    for seed in 0..seeds {
        let model = if seed % 2 == 0 {
            Model::Dmg
        } else {
            Model::Cgb
        };
        let (rom, end) = random_rom(seed, model == Model::Cgb);
        for hot_threshold in [0, 3] {
            assert_lockstep(&rom, model, hot_threshold, 200_000, Some(end));
        }
    }
}
