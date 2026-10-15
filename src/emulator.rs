use crate::cartridge::Cartridge;
use crate::cpu::CPU;
use crate::cpu::jit::Jit;
use crate::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};

pub const FRAME_M_CYCLES: usize = 17556;
pub const FRAMES_PER_SECOND: f64 = 4_194_304.0 / (FRAME_M_CYCLES as f64 * 4.0);

pub struct Emulator {
    cpu: CPU,
    jit: Option<Jit>,
    cycles: usize,
}

impl Emulator {
    pub fn new(cartridge: Cartridge, use_jit: bool, trace: bool) -> Emulator {
        let mut cpu = CPU::new(cartridge);
        cpu.trace = trace;
        let jit = (use_jit && !trace).then(|| Jit::new(&cpu));
        Emulator {
            cpu,
            jit,
            cycles: 0,
        }
    }

    pub fn set_serial_output(&mut self, enabled: bool) {
        self.cpu.mmu_mut().set_serial_output(enabled);
    }

    pub fn run_frame(&mut self) {
        while self.cycles < FRAME_M_CYCLES {
            self.cycles += match &mut self.jit {
                Some(jit) => jit.step(&mut self.cpu).ticks,
                None => self.cpu.cycle(),
            };
        }
        self.cycles -= FRAME_M_CYCLES;
    }

    pub fn frame(&self) -> &[u8; SCREEN_WIDTH * SCREEN_HEIGHT] {
        self.cpu.mmu().ppu.frame()
    }

    pub fn set_buttons(&mut self, pressed: u8) {
        self.cpu.mmu_mut().set_buttons(pressed);
    }

    pub fn take_samples(&mut self) -> Vec<f32> {
        self.cpu.mmu_mut().apu.take_samples()
    }

    pub fn save(&self) -> std::io::Result<()> {
        self.cpu.mmu().cartridge().save()
    }
}
