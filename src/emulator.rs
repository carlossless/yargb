use crate::cartridge::Cartridge;
use crate::cpu::CPU;
use crate::cpu::jit::Jit;
use crate::mmu::Model;
use crate::ppu::Frame;

pub const FRAME_DOTS: usize = 70224;
pub const FRAMES_PER_SECOND: f64 = 4_194_304.0 / FRAME_DOTS as f64;

pub struct Emulator {
    cpu: CPU,
    jit: Option<Jit>,
    dots: usize,
}

impl Emulator {
    pub fn new(cartridge: Cartridge, model: Model, use_jit: bool, trace: bool) -> Emulator {
        let mut cpu = CPU::new(cartridge, model);
        cpu.trace = trace;
        let jit = (use_jit && !trace).then(|| Jit::new(&cpu));
        Emulator { cpu, jit, dots: 0 }
    }

    pub fn set_serial_output(&mut self, enabled: bool) {
        self.cpu.mmu_mut().set_serial_output(enabled);
    }

    /// Runs until the PPU has drawn one frame's worth of dots.
    pub fn run_frame(&mut self) {
        while self.dots < FRAME_DOTS {
            let dots_per_m_cycle = self.cpu.mmu().dots_per_m_cycle();
            let m_cycles = match &mut self.jit {
                Some(jit) => jit.step(&mut self.cpu).ticks,
                None => self.cpu.cycle(),
            };
            self.dots += m_cycles * dots_per_m_cycle;
        }
        self.dots -= FRAME_DOTS;
    }

    pub fn frame(&self) -> &Frame {
        self.cpu.mmu().ppu.frame()
    }

    pub fn renders_color(&self) -> bool {
        self.cpu.mmu().ppu.renders_color()
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
