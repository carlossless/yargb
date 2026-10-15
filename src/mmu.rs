use crate::apu::Apu;
use crate::cartridge::Cartridge;
use crate::ppu::Ppu;
use crate::timer::Timer;

const TIMER_INTERRUPT: u8 = 1 << 2;
const SERIAL_INTERRUPT: u8 = 1 << 3;
const JOYPAD_INTERRUPT: u8 = 1 << 4;

const SERIAL_TRANSFER_CYCLES: u32 = 4096;
const FRAME_SEQUENCER_PERIOD: usize = 8192;

pub fn is_io(addr: u16) -> bool {
    matches!(addr, 0xFF00..=0xFF7F | 0xFFFF)
}

/// Whether a write to `addr` must see the PPU caught up to the current cycle.
pub fn needs_sync_on_write(addr: u16) -> bool {
    is_io(addr) || matches!(addr, 0x8000..=0x9FFF | 0xFE00..=0xFE9F)
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

pub struct MMU {
    cartridge: Cartridge,
    pub(crate) ppu: Ppu,
    pub(crate) apu: Apu,
    pub(crate) work_ram: [u8; 0x2000],
    pub(crate) high_ram: [u8; 0x7F],
    timer: Timer,
    interupt_flags: u8,
    interupt_enable: u8,
    joypad_select: u8,
    buttons: u8,
    serial_data: u8,
    serial_control: u8,
    serial_cycles: u32,
    serial_output: bool,
    dma_source: u8,
    exit_request: bool,
    pub(crate) code_refs: [u8; 0x10000],
    dirty_code: Vec<u16>,
}

impl MMU {
    pub fn new(cartridge: Cartridge) -> MMU {
        MMU {
            cartridge,
            ppu: Ppu::new(),
            apu: Apu::new(),
            work_ram: [0; 0x2000],
            high_ram: [0; 0x7F],
            timer: Timer::new(),
            interupt_flags: 0x01,
            interupt_enable: 0x00,
            joypad_select: 0x30,
            buttons: 0,
            serial_data: 0,
            serial_control: 0,
            serial_cycles: 0,
            serial_output: false,
            dma_source: 0xFF,
            exit_request: false,
            code_refs: [0; 0x10000],
            dirty_code: Vec::new(),
        }
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

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => self.cartridge.read_rom(addr),
            0x8000..=0x9FFF => self.ppu.read_vram(addr),
            0xA000..=0xBFFF => self.cartridge.read_ram(addr),
            0xC000..=0xDFFF => self.work_ram[addr as usize - 0xC000],
            0xE000..=0xFDFF => self.work_ram[addr as usize - 0xE000],
            0xFE00..=0xFE9F => self.ppu.read_oam(addr),
            0xFEA0..=0xFEFF => 0x00,
            0xFF00 => self.joypad(),
            0xFF01 => self.serial_data,
            0xFF02 => self.serial_control | 0x7E,
            0xFF04..=0xFF07 => self.timer.read_byte(addr),
            0xFF0F => self.interupt_flags | 0xE0,
            0xFF10..=0xFF3F => self.apu.read(addr),
            0xFF46 => self.dma_source,
            0xFF40..=0xFF4B => self.ppu.read_register(addr),
            0xFF80..=0xFFFE => self.high_ram[addr as usize - 0xFF80],
            0xFFFF => self.interupt_enable,
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
            0xC000..=0xDFFF => self.work_ram[addr as usize - 0xC000] = value,
            0xE000..=0xFDFF => self.work_ram[addr as usize - 0xE000] = value,
            0xFE00..=0xFE9F => self.ppu.write_oam(addr, value),
            0xFF00 => self.joypad_select = value & 0x30,
            0xFF01 => self.serial_data = value,
            0xFF02 => {
                self.serial_control = value & 0x81;
                if value & 0x81 == 0x81 {
                    if self.serial_output {
                        eprint!("{}", self.serial_data as char);
                    }
                    self.serial_cycles = SERIAL_TRANSFER_CYCLES;
                }
            }
            0xFF04 => {
                let frame_sequencer_edge = self.timer.counter() & 0x1000 != 0;
                if self.timer.write_byte(addr, value) {
                    self.interupt_flags |= TIMER_INTERRUPT;
                }
                if frame_sequencer_edge {
                    self.apu.step_frame_sequencer();
                }
            }
            0xFF05..=0xFF07 => {
                if self.timer.write_byte(addr, value) {
                    self.interupt_flags |= TIMER_INTERRUPT;
                }
            }
            0xFF0F => self.interupt_flags = value & 0x1F,
            0xFF10..=0xFF3F => self.apu.write(addr, value),
            0xFF46 => {
                self.dma_source = value;
                let source = (value as u16) << 8;
                for index in 0..0xA0 {
                    let byte = self.read_byte(source + index);
                    self.ppu.write_oam_index(index as usize, byte);
                }
            }
            0xFF40..=0xFF4B => self.interupt_flags |= self.ppu.write_register(addr, value),
            0xFF80..=0xFFFE => self.high_ram[addr as usize - 0xFF80] = value,
            0xFFFF => self.interupt_enable = value,
            _ => {}
        }
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
            && self.high_ram == other.high_ram
            && self.timer == other.timer
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
                self.serial_cycles,
                self.dma_source,
            ) == (
                other.serial_data,
                other.serial_control,
                other.serial_cycles,
                other.dma_source,
            )
    }

    pub fn tick(&mut self, m_cycles: usize) {
        let t_cycles = m_cycles * 4;
        let mut counter = self.timer.counter() as usize;
        if self.timer.tick(m_cycles) {
            self.interupt_flags |= TIMER_INTERRUPT;
        }
        self.interupt_flags |= self.ppu.tick(t_cycles);
        let mut remaining = t_cycles;
        while remaining > 0 {
            let step = remaining.min(FRAME_SEQUENCER_PERIOD - counter % FRAME_SEQUENCER_PERIOD);
            self.apu.tick(step);
            counter += step;
            remaining -= step;
            if counter % FRAME_SEQUENCER_PERIOD == 0 {
                self.apu.step_frame_sequencer();
            }
        }
        self.cartridge.tick(t_cycles);
        if self.serial_cycles > 0 {
            self.serial_cycles = self.serial_cycles.saturating_sub(t_cycles as u32);
            if self.serial_cycles == 0 {
                self.serial_data = 0xFF;
                self.serial_control &= 0x7F;
                self.interupt_flags |= SERIAL_INTERRUPT;
            }
        }
    }

    /// M-cycles until a component could raise an interrupt that IE enables.
    /// It may be early, never late.
    pub fn m_cycles_until_event(&self) -> usize {
        let enabled = self.interupt_enable;
        let mut m_cycles = self.ppu.t_cycles_until_interrupt(enabled).div_ceil(4);
        if enabled & TIMER_INTERRUPT != 0 {
            m_cycles = m_cycles.min(self.timer.m_cycles_until_overflow());
        }
        if enabled & SERIAL_INTERRUPT != 0 && self.serial_cycles > 0 {
            m_cycles = m_cycles.min((self.serial_cycles as usize).div_ceil(4));
        }
        m_cycles
    }

    pub fn pending_interrupts(&self) -> u8 {
        self.interupt_flags & self.interupt_enable & 0x1F
    }

    pub fn acknowledge_interrupt(&mut self, bit: u32) {
        self.interupt_flags &= !(1 << bit);
    }

    pub fn write_word(&mut self, addr: u16, value: u16) {
        self.write_byte(addr, (value & 0xFF) as u8);
        self.write_byte(addr.wrapping_add(1), (value >> 8) as u8);
    }
}
