use crate::apu::Apu;
use crate::cartridge::Cartridge;
use crate::ppu::Ppu;
use crate::timer::Timer;

const TIMER_INTERRUPT: u8 = 1 << 2;
const SERIAL_INTERRUPT: u8 = 1 << 3;
const JOYPAD_INTERRUPT: u8 = 1 << 4;

const SERIAL_CLOCK_PERIOD: usize = 512;
const SERIAL_FAST_CLOCK_PERIOD: usize = 16;
/// The serial bit clock leads the DIV counter by one M-cycle.
const SERIAL_CLOCK_PHASE: usize = 4;
const FRAME_SEQUENCER_PERIOD: usize = 8192;
/// M-cycles the CPU is stalled for each 16-byte block of a VRAM DMA, at normal speed.
const VRAM_DMA_BLOCK_CYCLES: usize = 8;
/// M-cycles the CPU is stalled while switching speed.
const SPEED_SWITCH_CYCLES: usize = 2050;

pub fn is_io(addr: u16) -> bool {
    matches!(addr, 0xFF00..=0xFF7F | 0xFFFF)
}

/// Whether an access to `addr` must see the hardware caught up to the current cycle.
pub fn needs_sync(addr: u16) -> bool {
    is_io(addr) || matches!(addr, 0x8000..=0x9FFF | 0xFE00..=0xFE9F)
}

/// Which console is emulated.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Model {
    Dmg,
    Cgb,
}

impl Model {
    /// The CGB for games whose header says they use its features, otherwise the DMG.
    pub fn for_cartridge(cartridge: &Cartridge) -> Model {
        if cartridge.supports_cgb() {
            Model::Cgb
        } else {
            Model::Dmg
        }
    }
}

/// Joypad buttons as bits of the state passed to `MMU::set_buttons`.
pub mod buttons {
    pub const RIGHT: u8 = 1 << 0;
    pub const LEFT: u8 = 1 << 1;
    pub const UP: u8 = 1 << 2;
    pub const DOWN: u8 = 1 << 3;
    pub const A: u8 = 1 << 4;
    pub const B: u8 = 1 << 5;
    pub const SELECT: u8 = 1 << 6;
    pub const START: u8 = 1 << 7;
}

#[derive(Clone, Copy, PartialEq)]
struct Dma {
    source: u16,
    index: u16,
}

#[derive(Clone, Copy, PartialEq)]
struct VramDma {
    source: u16,
    destination: u16,
    blocks: u8,
    on_hblank: bool,
}

pub struct MMU {
    cgb: bool,
    compat: bool,
    cartridge: Cartridge,
    pub(crate) ppu: Ppu,
    pub(crate) apu: Apu,
    pub(crate) work_ram: [u8; 0x8000],
    /// Added to `addr - 0xC000` to index `work_ram` for D000-DFFF, which is
    /// banked on the CGB.
    pub(crate) work_ram_bank_offset: u32,
    pub(crate) high_ram: [u8; 0x7F],
    timer: Timer,
    interupt_flags: u8,
    interupt_enable: u8,
    joypad_select: u8,
    buttons: u8,
    serial_data: u8,
    serial_control: u8,
    serial_bits: u8,
    serial_output: bool,
    dma_source: u8,
    dma: Option<Dma>,
    dma_requested: Option<(u16, bool)>,
    vram_dma: Option<VramDma>,
    vram_dma_source: u16,
    vram_dma_destination: u16,
    double_speed: bool,
    speed_switch_armed: bool,
    half_dot: bool,
    infrared: u8,
    undocumented: [u8; 4],
    stall_cycles: usize,
    cycle_t: usize,
    /// DIV, TIMA and TMA writes take effect at the end of their M-cycle, unlike
    /// other writes, which land on its third T-cycle.
    deferred_timer_write: Option<(u16, u8)>,
    exit_request: bool,
    pub(crate) code_refs: [u8; 0x10000],
    dirty_code: Vec<u16>,
}

impl MMU {
    /// A CGB runs cartridges that don't support it in a DMG compatibility mode,
    /// with its own registers locked and colours from the boot ROM's palettes.
    pub fn new(cartridge: Cartridge, model: Model) -> MMU {
        let cgb_hardware = model == Model::Cgb;
        let cgb = cgb_hardware && cartridge.supports_cgb();
        let compat = cgb_hardware && !cgb;
        MMU {
            cgb,
            compat,
            cartridge,
            ppu: Ppu::new(cgb, compat),
            apu: Apu::new(cgb_hardware),
            work_ram: [0; 0x8000],
            work_ram_bank_offset: 0,
            high_ram: [0; 0x7F],
            timer: Timer::new(if cgb_hardware { 0x2674 } else { 0xABC8 }),
            interupt_flags: 0x01,
            interupt_enable: 0x00,
            joypad_select: if cgb_hardware { 0x30 } else { 0x00 },
            buttons: 0,
            serial_data: 0,
            serial_control: 0,
            serial_bits: 0,
            serial_output: false,
            dma_source: 0xFF,
            dma: None,
            dma_requested: None,
            vram_dma: None,
            vram_dma_source: 0,
            vram_dma_destination: 0,
            double_speed: false,
            speed_switch_armed: false,
            half_dot: false,
            infrared: 0,
            undocumented: [0; 4],
            stall_cycles: 0,
            cycle_t: 0,
            deferred_timer_write: None,
            exit_request: false,
            code_refs: [0; 0x10000],
            dirty_code: Vec::new(),
        }
    }

    pub fn dots_per_m_cycle(&self) -> usize {
        if self.double_speed { 2 } else { 4 }
    }

    pub fn cartridge(&self) -> &Cartridge {
        &self.cartridge
    }

    pub fn set_serial_output(&mut self, enabled: bool) {
        self.serial_output = enabled;
    }

    pub fn set_buttons(&mut self, pressed: u8) {
        if pressed & !self.buttons != 0 {
            self.interupt_flags |= JOYPAD_INTERRUPT;
        }
        self.buttons = pressed;
    }

    pub fn any_button_pressed(&self) -> bool {
        self.buttons != 0
    }

    fn joypad(&self) -> u8 {
        let mut pressed = 0;
        if self.joypad_select & 0x10 == 0 {
            pressed |= self.buttons & 0x0F;
        }
        if self.joypad_select & 0x20 == 0 {
            pressed |= self.buttons >> 4;
        }
        0xC0 | self.joypad_select | !pressed & 0x0F
    }

    fn work_ram_index(&self, addr: u16) -> usize {
        let offset = (addr as usize - 0xC000) & 0x1FFF;
        if offset < 0x1000 {
            offset
        } else {
            offset + self.work_ram_bank_offset as usize
        }
    }

    fn select_work_ram_bank(&mut self, value: u8) {
        let bank = (value & 0x07).max(1) as u32;
        let offset = (bank - 1) * 0x1000;
        if offset == self.work_ram_bank_offset {
            return;
        }
        self.work_ram_bank_offset = offset;
        for addr in 0xD000..0xE000u16 {
            if self.code_refs[addr as usize] != 0 {
                self.dirty_code.push(addr);
            }
        }
        self.exit_request = true;
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => self.cartridge.read_rom(addr),
            0x8000..=0x9FFF => self.ppu.read_vram(addr),
            0xA000..=0xBFFF => self.cartridge.read_ram(addr),
            0xC000..=0xFDFF => self.work_ram[self.work_ram_index(addr)],
            0xFE00..=0xFE9F => self.ppu.read_oam(addr),
            0xFEA0..=0xFEFF => 0x00,
            0xFF00 => self.joypad(),
            0xFF01 => self.serial_data,
            0xFF02 => self.serial_control | if self.cgb { 0x7C } else { 0x7E },
            0xFF04..=0xFF07 => self.timer.read_byte(addr),
            0xFF0F => self.interupt_flags | 0xE0,
            0xFF10..=0xFF3F => self.apu.read(addr),
            0xFF46 => self.dma_source,
            0xFF40..=0xFF4B => self.ppu.read_register(addr),
            0xFF80..=0xFFFE => self.high_ram[addr as usize - 0xFF80],
            0xFFFF => self.interupt_enable,
            _ if self.cgb => self.read_cgb_register(addr),
            _ if self.compat => self.read_compat_register(addr),
            _ => 0xFF,
        }
    }

    fn read_compat_register(&self, addr: u16) -> u8 {
        match addr {
            0xFF4F => 0xFE,
            0xFF68 => 0xC8,
            0xFF6A => 0xD0,
            0xFF72 => self.undocumented[0],
            0xFF73 => self.undocumented[1],
            0xFF75 => self.undocumented[3] | 0x8F,
            0xFF76 => self.apu.pcm12(),
            0xFF77 => self.apu.pcm34(),
            _ => 0xFF,
        }
    }

    fn write_compat_register(&mut self, addr: u16, value: u8) {
        match addr {
            0xFF72 => self.undocumented[0] = value,
            0xFF73 => self.undocumented[1] = value,
            0xFF75 => self.undocumented[3] = value & 0x70,
            _ => {}
        }
    }

    fn read_cgb_register(&self, addr: u16) -> u8 {
        match addr {
            0xFF4D => 0x7E | (self.double_speed as u8) << 7 | self.speed_switch_armed as u8,
            0xFF4F | 0xFF68..=0xFF6C => self.ppu.read_cgb_register(addr),
            0xFF55 => match self.vram_dma {
                Some(dma) => dma.blocks - 1,
                None => 0xFF,
            },
            0xFF56 => self.infrared | 0x3E,
            0xFF70 => 0xF8 | (self.work_ram_bank_offset / 0x1000 + 1) as u8,
            0xFF72 => self.undocumented[0],
            0xFF73 => self.undocumented[1],
            0xFF74 => self.undocumented[2],
            0xFF75 => self.undocumented[3] | 0x8F,
            0xFF76 => self.apu.pcm12(),
            0xFF77 => self.apu.pcm34(),
            _ => 0xFF,
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        (self.read_byte(addr) as u16) | ((self.read_byte(addr.wrapping_add(1)) as u16) << 8)
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        if is_io(addr) || addr < 0x8000 {
            self.exit_request = true;
        }
        let code_addr = if (0xE000..0xFE00).contains(&addr) {
            addr - 0x2000
        } else {
            addr
        };
        if self.code_refs[code_addr as usize] != 0 {
            self.exit_request = true;
            self.dirty_code.push(code_addr);
        }
        match addr {
            0x0000..=0x7FFF => self.cartridge.write_rom(addr, value),
            0x8000..=0x9FFF => self.ppu.write_vram(addr, value),
            0xA000..=0xBFFF => self.cartridge.write_ram(addr, value),
            0xC000..=0xFDFF => {
                let index = self.work_ram_index(addr);
                self.work_ram[index] = value;
            }
            0xFE00..=0xFE9F => self.ppu.write_oam(addr, value),
            0xFF00 => self.joypad_select = value & 0x30,
            0xFF01 => self.serial_data = value,
            0xFF02 => {
                self.serial_control = value & if self.cgb { 0x83 } else { 0x81 };
                if value & 0x81 == 0x81 {
                    if self.serial_output {
                        eprint!("{}", self.serial_data as char);
                    }
                    self.serial_bits = 8;
                }
            }
            0xFF04..=0xFF06 if self.cycle_t > 0 => self.deferred_timer_write = Some((addr, value)),
            0xFF04 => self.reset_div(),
            0xFF05..=0xFF07 => self.timer.write_byte(addr, value),
            0xFF0F => self.interupt_flags = value & 0x1F,
            0xFF10..=0xFF3F => self.apu.write(addr, value),
            0xFF46 => {
                self.dma_source = value;
                let source = (value as u16) << 8;
                let source = if source >= 0xE000 {
                    source - 0x2000
                } else {
                    source
                };
                self.dma_requested = Some((source, false));
            }
            0xFF40..=0xFF4B => self.interupt_flags |= self.ppu.write_register(addr, value),
            0xFF80..=0xFFFE => self.high_ram[addr as usize - 0xFF80] = value,
            0xFFFF => self.interupt_enable = value,
            _ if self.cgb => self.write_cgb_register(addr, value),
            _ if self.compat => self.write_compat_register(addr, value),
            _ => {}
        }
    }

    fn write_cgb_register(&mut self, addr: u16, value: u8) {
        match addr {
            0xFF4D => self.speed_switch_armed = value & 1 != 0,
            0xFF4F | 0xFF68..=0xFF6C => self.ppu.write_cgb_register(addr, value),
            0xFF51 => self.vram_dma_source = self.vram_dma_source & 0x00FF | (value as u16) << 8,
            0xFF52 => self.vram_dma_source = self.vram_dma_source & 0xFF00 | (value & 0xF0) as u16,
            0xFF53 => {
                self.vram_dma_destination =
                    self.vram_dma_destination & 0x00FF | ((value & 0x1F) as u16) << 8
            }
            0xFF54 => {
                self.vram_dma_destination =
                    self.vram_dma_destination & 0xFF00 | (value & 0xF0) as u16
            }
            0xFF55 => self.start_vram_dma(value),
            0xFF56 => self.infrared = value & 0xC1,
            0xFF70 => self.select_work_ram_bank(value),
            0xFF72 => self.undocumented[0] = value,
            0xFF73 => self.undocumented[1] = value,
            0xFF74 => self.undocumented[2] = value,
            0xFF75 => self.undocumented[3] = value & 0x70,
            _ => {}
        }
    }

    fn start_vram_dma(&mut self, value: u8) {
        if let Some(dma) = self.vram_dma
            && value & 0x80 == 0
        {
            self.vram_dma = None;
            self.vram_dma_source = dma.source;
            self.vram_dma_destination = dma.destination;
            return;
        }
        let dma = VramDma {
            source: self.vram_dma_source,
            destination: self.vram_dma_destination,
            blocks: (value & 0x7F) + 1,
            on_hblank: value & 0x80 != 0,
        };
        self.vram_dma = Some(dma);
        if !dma.on_hblank {
            while self.vram_dma.is_some() {
                self.copy_vram_dma_block();
            }
        }
    }

    fn copy_vram_dma_block(&mut self) {
        let Some(mut dma) = self.vram_dma else {
            return;
        };
        for offset in 0..16 {
            let byte = self.read_byte(dma.source.wrapping_add(offset));
            self.ppu
                .write_vram(0x8000 | (dma.destination + offset) & 0x1FFF, byte);
        }
        dma.source = dma.source.wrapping_add(16);
        dma.destination = (dma.destination + 16) & 0x1FF0;
        dma.blocks -= 1;
        self.stall_cycles += VRAM_DMA_BLOCK_CYCLES << self.double_speed as usize;
        self.vram_dma = (dma.blocks > 0).then_some(dma);
        self.vram_dma_source = dma.source;
        self.vram_dma_destination = dma.destination;
    }

    /// Handles STOP. With a speed switch armed on the CGB, it switches speed
    /// and returns the M-cycles the switch stalls the CPU for; otherwise it
    /// returns `None` and the CPU stops until a button is pressed.
    pub fn stop(&mut self) -> Option<usize> {
        self.reset_div();
        if self.cgb && self.speed_switch_armed {
            self.speed_switch_armed = false;
            self.double_speed = !self.double_speed;
            return Some(SPEED_SWITCH_CYCLES);
        }
        None
    }

    pub fn stall(&mut self, m_cycles: usize) {
        self.stall_cycles += m_cycles;
    }

    pub fn take_stall_cycles(&mut self) -> usize {
        std::mem::take(&mut self.stall_cycles)
    }

    pub fn stall_pending(&self) -> bool {
        self.stall_cycles > 0
    }

    pub fn rom_offset(&self, addr: u16) -> usize {
        self.cartridge.rom_offset(addr)
    }

    pub fn take_exit_request(&mut self) -> bool {
        std::mem::take(&mut self.exit_request)
    }

    pub fn track_code(&mut self, range: std::ops::Range<usize>) {
        for refs in &mut self.code_refs[range] {
            *refs += 1;
        }
    }

    pub fn untrack_code(&mut self, range: std::ops::Range<usize>) {
        for refs in &mut self.code_refs[range] {
            *refs -= 1;
        }
    }

    pub fn take_dirty_code(&mut self) -> Vec<u16> {
        std::mem::take(&mut self.dirty_code)
    }

    #[cfg(test)]
    pub fn same_state(&self, other: &MMU) -> bool {
        self.cartridge == other.cartridge
            && self.ppu == other.ppu
            && self.apu == other.apu
            && self.work_ram == other.work_ram
            && self.work_ram_bank_offset == other.work_ram_bank_offset
            && self.high_ram == other.high_ram
            && self.timer == other.timer
            && (self.dma, self.dma_requested) == (other.dma, other.dma_requested)
            && (
                self.vram_dma,
                self.vram_dma_source,
                self.vram_dma_destination,
            ) == (
                other.vram_dma,
                other.vram_dma_source,
                other.vram_dma_destination,
            )
            && (self.double_speed, self.speed_switch_armed, self.half_dot)
                == (other.double_speed, other.speed_switch_armed, other.half_dot)
            && (self.infrared, self.undocumented, self.stall_cycles)
                == (other.infrared, other.undocumented, other.stall_cycles)
            && (
                self.interupt_flags,
                self.interupt_enable,
                self.joypad_select,
                self.buttons,
            ) == (
                other.interupt_flags,
                other.interupt_enable,
                other.joypad_select,
                other.buttons,
            )
            && (
                self.serial_data,
                self.serial_control,
                self.serial_bits,
                self.dma_source,
            ) == (
                other.serial_data,
                other.serial_control,
                other.serial_bits,
                other.dma_source,
            )
    }

    pub fn dma_active(&self) -> bool {
        self.dma.is_some() || self.dma_requested.is_some()
    }

    fn bus_blocked(&self, addr: u16) -> bool {
        let Some(dma) = self.dma else {
            return false;
        };
        let video_bus = |addr: u16| (0x8000..=0x9FFF).contains(&addr);
        match addr {
            0xFE00..=0xFEFF => true,
            0xFF00..=0xFFFF => false,
            _ => video_bus(addr) == video_bus(dma.source),
        }
    }

    fn ppu_blocked(&self, addr: u16, write: bool) -> bool {
        match addr {
            0x8000..=0x9FFF => !self.ppu.vram_accessible(),
            0xFE00..=0xFE9F if write => !self.ppu.oam_writable(),
            0xFE00..=0xFE9F => !self.ppu.oam_readable(),
            _ => false,
        }
    }

    pub fn cpu_read(&self, addr: u16) -> u8 {
        if self.bus_blocked(addr) || self.ppu_blocked(addr, false) {
            return 0xFF;
        }
        self.read_byte(addr)
    }

    pub fn cpu_write(&mut self, addr: u16, value: u8) {
        if self.bus_blocked(addr) || self.ppu_blocked(addr, true) {
            return;
        }
        self.write_byte(addr, value);
    }

    fn copy_dma_byte(&mut self) {
        if let Some(dma) = &mut self.dma {
            let (source, index) = (dma.source, dma.index);
            if index == 0xA0 {
                self.dma = None;
                return;
            }
            dma.index += 1;
            let byte = self.read_byte(source + index);
            self.ppu.write_oam_index(index as usize, byte);
        }
    }

    fn step_dma(&mut self) {
        match self.dma_requested {
            Some((source, true)) => {
                self.dma_requested = None;
                self.dma = Some(Dma { source, index: 0 });
            }
            Some((source, false)) => {
                self.dma_requested = Some((source, true));
                self.copy_dma_byte();
                return;
            }
            None => {}
        }
        self.copy_dma_byte();
    }

    /// Finishes an M-cycle that `begin_cycle` started, then advances by
    /// `m_cycles` more.
    pub fn tick(&mut self, mut m_cycles: usize) {
        self.finish_cycle();
        while m_cycles > 0 && self.dma_active() {
            self.step_dma();
            self.tick_components(4);
            m_cycles -= 1;
        }
        if m_cycles > 0 {
            self.tick_components(m_cycles * 4);
        }
    }

    /// Starts a new M-cycle and runs its first `t_cycles` T-cycles, which is
    /// where a CPU write lands. The next `tick` finishes the cycle.
    pub fn begin_cycle(&mut self, t_cycles: usize) {
        self.finish_cycle();
        if self.dma_active() {
            self.step_dma();
        }
        self.tick_components(t_cycles);
        self.cycle_t = t_cycles;
    }

    fn reset_div(&mut self) {
        let counter = self.timer.counter();
        self.timer.write_byte(0xFF04, 0);
        if counter & self.frame_sequencer_bit() != 0 {
            self.apu.step_frame_sequencer();
        }
        if (counter as usize + SERIAL_CLOCK_PHASE) & 0x0100 != 0 {
            self.clock_serial(1);
        }
    }

    pub fn finish_cycle(&mut self) {
        if self.cycle_t > 0 {
            self.tick_components(4 - self.cycle_t);
            self.cycle_t = 0;
            if let Some((addr, value)) = self.deferred_timer_write.take() {
                self.write_byte(addr, value);
            }
        }
    }

    fn frame_sequencer_bit(&self) -> u16 {
        if self.double_speed { 0x2000 } else { 0x1000 }
    }

    fn serial_clock_period(&self) -> usize {
        if self.cgb && self.serial_control & 0x02 != 0 {
            SERIAL_FAST_CLOCK_PERIOD
        } else {
            SERIAL_CLOCK_PERIOD
        }
    }

    /// Advances by CPU T-cycles. The timer and serial port run at the CPU's
    /// clock; the PPU, APU and cartridge clock run at half of it in double speed.
    fn tick_components(&mut self, t_cycles: usize) {
        let start = self.timer.counter() as usize;
        if self.timer.tick(t_cycles) {
            self.interupt_flags |= TIMER_INTERRUPT;
        }
        let start_half_dot = self.half_dot;
        let dots = self.dots_for(t_cycles);
        self.interupt_flags |= self.ppu.tick(dots);
        if self.ppu.take_hblank_started() && self.vram_dma.is_some_and(|dma| dma.on_hblank) {
            self.copy_vram_dma_block();
        }

        let end_half_dot = self.half_dot;
        self.half_dot = start_half_dot;
        let period = FRAME_SEQUENCER_PERIOD << self.double_speed as usize;
        let mut counter = start;
        let mut remaining = t_cycles;
        while remaining > 0 {
            let step = remaining.min(period - counter % period);
            let step_dots = self.dots_for(step);
            self.apu.tick(step_dots);
            counter += step;
            remaining -= step;
            if counter % period == 0 {
                self.apu.step_frame_sequencer();
            }
        }
        debug_assert_eq!(self.half_dot, end_half_dot);
        self.cartridge.tick(dots);

        let period = self.serial_clock_period();
        let serial = start + SERIAL_CLOCK_PHASE;
        let edges = (serial + t_cycles) / period - serial / period;
        self.clock_serial(edges);
    }

    /// Converts CPU T-cycles to PPU dots, carrying the half dot double speed
    /// leaves over.
    fn dots_for(&mut self, t_cycles: usize) -> usize {
        if !self.double_speed {
            return t_cycles;
        }
        let total = t_cycles + self.half_dot as usize;
        self.half_dot = total % 2 == 1;
        total / 2
    }

    fn clock_serial(&mut self, edges: usize) {
        if self.serial_bits == 0 || edges == 0 {
            return;
        }
        self.serial_bits = self.serial_bits.saturating_sub(edges.min(8) as u8);
        if self.serial_bits == 0 {
            self.serial_data = 0xFF;
            self.serial_control &= 0x7F;
            self.interupt_flags |= SERIAL_INTERRUPT;
        }
    }

    fn dots_to_m_cycles(&self, dots: usize) -> usize {
        if dots == usize::MAX {
            return usize::MAX;
        }
        if self.double_speed {
            dots.div_ceil(2)
        } else {
            dots.div_ceil(4)
        }
    }

    /// M-cycles until something could happen that a compiled block must not
    /// run past: an interrupt IE enables (when `interrupts` is set) or a block
    /// of an HBlank VRAM DMA. It may be early, never late.
    pub fn m_cycles_until_event(&self, interrupts: bool) -> usize {
        let mut m_cycles = usize::MAX;
        if self.vram_dma.is_some_and(|dma| dma.on_hblank) {
            m_cycles = self.dots_to_m_cycles(self.ppu.t_cycles_until_hblank());
        }
        if !interrupts {
            return m_cycles;
        }
        let enabled = self.interupt_enable;
        m_cycles = m_cycles.min(self.dots_to_m_cycles(self.ppu.t_cycles_until_interrupt(enabled)));
        if enabled & TIMER_INTERRUPT != 0 {
            m_cycles = m_cycles.min(self.timer.m_cycles_until_overflow());
        }
        if enabled & SERIAL_INTERRUPT != 0 && self.serial_bits > 0 {
            let period = self.serial_clock_period();
            let counter = self.timer.counter() as usize + SERIAL_CLOCK_PHASE;
            let t_cycles = period - counter % period + (self.serial_bits as usize - 1) * period;
            m_cycles = m_cycles.min(t_cycles.div_ceil(4));
        }
        m_cycles
    }

    pub fn pending_interrupts(&self) -> u8 {
        self.interupt_flags & self.interupt_enable & 0x1F
    }

    pub fn acknowledge_interrupt(&mut self, bit: u32) {
        self.interupt_flags &= !(1 << bit);
    }
}
