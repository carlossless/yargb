use std::fs;
use std::path::PathBuf;

const ROM_BANK: usize = 0x4000;
const RAM_BANK: usize = 0x2000;
const T_CYCLES_PER_SECOND: u64 = 4_194_304;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Mbc {
    None,
    Mbc1,
    Mbc2,
    Mbc3,
    Mbc5,
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
struct Clock {
    seconds: u8,
    minutes: u8,
    hours: u8,
    days: u16,
    halted: bool,
    carry: bool,
}

impl Clock {
    fn advance(&mut self, seconds: u64) {
        if self.halted || seconds == 0 {
            return;
        }
        let total = self.seconds as u64
            + self.minutes as u64 * 60
            + self.hours as u64 * 3600
            + self.days as u64 * 86400
            + seconds;
        self.seconds = (total % 60) as u8;
        self.minutes = (total / 60 % 60) as u8;
        self.hours = (total / 3600 % 24) as u8;
        let days = total / 86400;
        if days > 0x1FF {
            self.carry = true;
        }
        self.days = (days & 0x1FF) as u16;
    }

    fn read(&self, register: u8) -> u8 {
        match register {
            0x08 => self.seconds,
            0x09 => self.minutes,
            0x0A => self.hours,
            0x0B => self.days as u8,
            _ => (self.days >> 8) as u8 & 1 | (self.halted as u8) << 6 | (self.carry as u8) << 7,
        }
    }

    fn write(&mut self, register: u8, value: u8) {
        match register {
            0x08 => self.seconds = value & 0x3F,
            0x09 => self.minutes = value & 0x3F,
            0x0A => self.hours = value & 0x1F,
            0x0B => self.days = self.days & 0x100 | value as u16,
            _ => {
                self.days = self.days & 0xFF | (value as u16 & 1) << 8;
                self.halted = value & 0x40 != 0;
                self.carry = value & 0x80 != 0;
            }
        }
    }
}

/// A cartridge with its memory bank controller, external RAM and, for MBC3,
/// the real-time clock, which advances with emulated time.
#[derive(Debug, PartialEq)]
pub struct Cartridge {
    rom: Vec<u8>,
    ram: Vec<u8>,
    mbc: Mbc,
    ram_enabled: bool,
    rom_bank: usize,
    upper_bank: usize,
    advanced_banking: bool,
    clock: Clock,
    latched_clock: Clock,
    latch_armed: bool,
    clock_cycles: u64,
    save_path: Option<PathBuf>,
    cgb_flag: u8,
}

impl Cartridge {
    pub fn new(mut rom: Vec<u8>, save_path: Option<PathBuf>) -> Cartridge {
        let header = |offset: usize| rom.get(offset).copied().unwrap_or(0);
        let kind = header(0x147);
        let cgb_flag = header(0x143);
        let (mbc, battery) = match kind {
            0x00 | 0x08 => (Mbc::None, false),
            0x09 => (Mbc::None, true),
            0x01 | 0x02 => (Mbc::Mbc1, false),
            0x03 => (Mbc::Mbc1, true),
            0x05 => (Mbc::Mbc2, false),
            0x06 => (Mbc::Mbc2, true),
            0x11 | 0x12 => (Mbc::Mbc3, false),
            0x0F | 0x10 | 0x13 => (Mbc::Mbc3, true),
            0x19 | 0x1A | 0x1C | 0x1D => (Mbc::Mbc5, false),
            0x1B | 0x1E => (Mbc::Mbc5, true),
            other => panic!("cartridge type {other:#04X} is not supported"),
        };
        let has_ram = matches!(
            kind,
            0x02 | 0x03 | 0x08 | 0x09 | 0x10 | 0x12 | 0x13 | 0x1A | 0x1B | 0x1D | 0x1E
        );
        let ram_size = match (mbc, header(0x149)) {
            (Mbc::Mbc2, _) => 0x200,
            (_, 1) => 0x800,
            (_, 2) => 0x2000,
            (_, 3) => 0x8000,
            (_, 4) => 0x20000,
            (_, 5) => 0x10000,
            // Some cartridges with RAM declare a size of 0; they still get a bank.
            _ if has_ram => 0x2000,
            _ => 0,
        };
        let rom_size = rom.len().next_power_of_two().max(2 * ROM_BANK);
        rom.resize(rom_size, 0xFF);

        let mut ram = vec![0; ram_size];
        if battery && let Some(saved) = save_path.as_ref().and_then(|path| fs::read(path).ok()) {
            let len = saved.len().min(ram.len());
            ram[..len].copy_from_slice(&saved[..len]);
        }

        Cartridge {
            rom,
            ram,
            mbc,
            ram_enabled: false,
            rom_bank: 1,
            upper_bank: 0,
            advanced_banking: false,
            clock: Clock::default(),
            latched_clock: Clock::default(),
            latch_armed: false,
            clock_cycles: 0,
            save_path: save_path.filter(|_| battery),
            cgb_flag,
        }
    }

    #[cfg(test)]
    pub fn ram(&self) -> &[u8] {
        &self.ram
    }

    pub fn save(&self) -> std::io::Result<()> {
        match &self.save_path {
            Some(path) if !self.ram.is_empty() => fs::write(path, &self.ram),
            _ => Ok(()),
        }
    }

    /// Whether the header marks the game as using Game Boy Color features.
    pub fn supports_cgb(&self) -> bool {
        self.cgb_flag & 0x80 != 0
    }

    pub fn rom_len(&self) -> usize {
        self.rom.len()
    }

    fn rom_banks(&self) -> usize {
        self.rom.len() / ROM_BANK
    }

    pub fn rom_offset(&self, addr: u16) -> usize {
        let bank = match (self.mbc, addr) {
            (Mbc::Mbc1, 0x0000..=0x3FFF) if self.advanced_banking => self.upper_bank << 5,
            (_, 0x0000..=0x3FFF) => 0,
            (Mbc::Mbc1, _) => self.upper_bank << 5 | self.rom_bank,
            _ => self.rom_bank,
        };
        (bank & (self.rom_banks() - 1)) * ROM_BANK + (addr as usize & (ROM_BANK - 1))
    }

    pub fn read_rom(&self, addr: u16) -> u8 {
        self.rom[self.rom_offset(addr)]
    }

    pub fn write_rom(&mut self, addr: u16, value: u8) {
        match (self.mbc, addr) {
            (Mbc::None, _) => {}
            (Mbc::Mbc2, 0x0000..=0x3FFF) => {
                if addr & 0x100 == 0 {
                    self.ram_enabled = value & 0x0F == 0x0A;
                } else {
                    self.rom_bank = (value as usize & 0x0F).max(1);
                }
            }
            (Mbc::Mbc2, _) => {}
            (_, 0x0000..=0x1FFF) => self.ram_enabled = value & 0x0F == 0x0A,
            (Mbc::Mbc1, 0x2000..=0x3FFF) => self.rom_bank = (value as usize & 0x1F).max(1),
            (Mbc::Mbc3, 0x2000..=0x3FFF) => self.rom_bank = (value as usize & 0x7F).max(1),
            (Mbc::Mbc5, 0x2000..=0x2FFF) => self.rom_bank = self.rom_bank & 0x100 | value as usize,
            (Mbc::Mbc5, 0x3000..=0x3FFF) => {
                self.rom_bank = self.rom_bank & 0xFF | (value as usize & 1) << 8
            }
            (Mbc::Mbc1, 0x4000..=0x5FFF) => self.upper_bank = value as usize & 0x03,
            (Mbc::Mbc3, 0x4000..=0x5FFF) => self.upper_bank = value as usize & 0x0F,
            (Mbc::Mbc5, 0x4000..=0x5FFF) => self.upper_bank = value as usize & 0x0F,
            (Mbc::Mbc1, 0x6000..=0x7FFF) => self.advanced_banking = value & 1 != 0,
            (Mbc::Mbc3, 0x6000..=0x7FFF) => {
                if self.latch_armed && value == 1 {
                    self.latched_clock = self.clock;
                }
                self.latch_armed = value == 0;
            }
            _ => {}
        }
    }

    fn ram_offset(&self, addr: u16) -> Option<usize> {
        if !self.ram_enabled || self.ram.is_empty() {
            return None;
        }
        let offset = addr as usize - 0xA000;
        let bank = match self.mbc {
            Mbc::Mbc1 if self.advanced_banking => self.upper_bank,
            Mbc::Mbc1 | Mbc::Mbc2 | Mbc::None => 0,
            _ => self.upper_bank,
        };
        Some((bank * RAM_BANK + offset) % self.ram.len())
    }

    fn clock_register(&self) -> Option<u8> {
        let register = self.upper_bank as u8;
        (self.mbc == Mbc::Mbc3 && self.ram_enabled && (0x08..=0x0C).contains(&register))
            .then_some(register)
    }

    pub fn read_ram(&self, addr: u16) -> u8 {
        if let Some(register) = self.clock_register() {
            return self.latched_clock.read(register);
        }
        match self.ram_offset(addr) {
            Some(offset) if self.mbc == Mbc::Mbc2 => self.ram[offset & 0x1FF] | 0xF0,
            Some(offset) => self.ram[offset],
            None => 0xFF,
        }
    }

    pub fn write_ram(&mut self, addr: u16, value: u8) {
        if let Some(register) = self.clock_register() {
            self.clock.write(register, value);
            return;
        }
        match self.ram_offset(addr) {
            Some(offset) if self.mbc == Mbc::Mbc2 => self.ram[offset & 0x1FF] = value & 0x0F,
            Some(offset) => self.ram[offset] = value,
            None => {}
        }
    }

    pub fn tick(&mut self, t_cycles: usize) {
        if self.mbc != Mbc::Mbc3 {
            return;
        }
        self.clock_cycles += t_cycles as u64;
        let seconds = self.clock_cycles / T_CYCLES_PER_SECOND;
        self.clock_cycles %= T_CYCLES_PER_SECOND;
        self.clock.advance(seconds);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A ROM whose every bank starts with its own number.
    fn cartridge(kind: u8, rom_banks: usize, ram_size: u8) -> Cartridge {
        let mut rom = vec![0; rom_banks * ROM_BANK];
        for bank in 0..rom_banks {
            rom[bank * ROM_BANK] = bank as u8;
        }
        rom[0x147] = kind;
        rom[0x149] = ram_size;
        Cartridge::new(rom, None)
    }

    fn bank_at(cartridge: &Cartridge, addr: u16) -> u8 {
        cartridge.read_rom(addr & 0xC000)
    }

    #[test]
    fn rom_only_cartridges_ignore_bank_writes() {
        let mut cartridge = cartridge(0x00, 2, 0);
        cartridge.write_rom(0x2000, 5);
        assert_eq!(bank_at(&cartridge, 0x4000), 1);
        assert_eq!(cartridge.read_ram(0xA000), 0xFF);
    }

    #[test]
    fn mbc1_switches_rom_banks_and_maps_bank_0_to_1() {
        let mut cartridge = cartridge(0x01, 64, 0);
        cartridge.write_rom(0x2000, 0);
        assert_eq!(bank_at(&cartridge, 0x4000), 1);
        cartridge.write_rom(0x2000, 0x13);
        assert_eq!(bank_at(&cartridge, 0x4000), 0x13);
        cartridge.write_rom(0x4000, 1);
        assert_eq!(bank_at(&cartridge, 0x4000), 0x33);
        assert_eq!(bank_at(&cartridge, 0x0000), 0);
        cartridge.write_rom(0x6000, 1);
        assert_eq!(bank_at(&cartridge, 0x0000), 0x20);
    }

    #[test]
    fn rom_banks_wrap_to_the_rom_size() {
        let mut cartridge = cartridge(0x01, 4, 0);
        cartridge.write_rom(0x2000, 6);
        assert_eq!(bank_at(&cartridge, 0x4000), 2);
    }

    #[test]
    fn ram_needs_enabling() {
        let mut cartridge = cartridge(0x03, 2, 2);
        cartridge.write_ram(0xA000, 0x42);
        assert_eq!(cartridge.read_ram(0xA000), 0xFF);
        cartridge.write_rom(0x0000, 0x0A);
        cartridge.write_ram(0xA000, 0x42);
        assert_eq!(cartridge.read_ram(0xA000), 0x42);
        cartridge.write_rom(0x0000, 0x00);
        assert_eq!(cartridge.read_ram(0xA000), 0xFF);
    }

    #[test]
    fn mbc1_ram_banks_need_advanced_banking() {
        let mut cartridge = cartridge(0x03, 2, 3);
        cartridge.write_rom(0x0000, 0x0A);
        cartridge.write_rom(0x4000, 2);
        cartridge.write_ram(0xA000, 1);
        cartridge.write_rom(0x6000, 1);
        cartridge.write_ram(0xA000, 2);
        assert_eq!(cartridge.ram()[0], 1);
        assert_eq!(cartridge.ram()[2 * RAM_BANK], 2);
    }

    #[test]
    fn ram_declared_without_a_size_still_gets_a_bank() {
        assert_eq!(cartridge(0x02, 2, 0).ram().len(), RAM_BANK);
        assert!(cartridge(0x01, 2, 0).ram().is_empty());
    }

    #[test]
    fn mbc2_has_512_half_bytes_of_ram() {
        let mut cartridge = cartridge(0x06, 4, 0);
        cartridge.write_rom(0x0000, 0x0A);
        cartridge.write_ram(0xA000, 0xAB);
        assert_eq!(cartridge.read_ram(0xA000), 0xFB);
        assert_eq!(cartridge.read_ram(0xA200), 0xFB);
        cartridge.write_rom(0x0100, 3);
        assert_eq!(bank_at(&cartridge, 0x4000), 3);
    }

    #[test]
    fn mbc3_clock_counts_and_latches() {
        let mut cartridge = cartridge(0x10, 2, 3);
        cartridge.write_rom(0x0000, 0x0A);
        cartridge.tick(T_CYCLES_PER_SECOND as usize * 61);
        cartridge.write_rom(0x6000, 0);
        cartridge.write_rom(0x6000, 1);
        cartridge.write_rom(0x4000, 0x08);
        assert_eq!(cartridge.read_ram(0xA000), 1);
        cartridge.write_rom(0x4000, 0x09);
        assert_eq!(cartridge.read_ram(0xA000), 1);
        cartridge.tick(T_CYCLES_PER_SECOND as usize);
        cartridge.write_rom(0x4000, 0x08);
        assert_eq!(
            cartridge.read_ram(0xA000),
            1,
            "latched value until the next latch"
        );
    }

    #[test]
    fn mbc3_clock_stops_when_halted() {
        let mut cartridge = cartridge(0x10, 2, 3);
        cartridge.write_rom(0x0000, 0x0A);
        cartridge.write_rom(0x4000, 0x0C);
        cartridge.write_ram(0xA000, 0x40);
        cartridge.tick(T_CYCLES_PER_SECOND as usize * 10);
        cartridge.write_rom(0x6000, 0);
        cartridge.write_rom(0x6000, 1);
        cartridge.write_rom(0x4000, 0x08);
        assert_eq!(cartridge.read_ram(0xA000), 0);
    }

    #[test]
    fn mbc5_has_nine_bit_rom_banks_and_bank_0() {
        let mut cartridge = cartridge(0x19, 512, 0);
        cartridge.write_rom(0x2000, 0);
        assert_eq!(bank_at(&cartridge, 0x4000), 0);
        cartridge.write_rom(0x2000, 0x34);
        cartridge.write_rom(0x3000, 1);
        assert_eq!(cartridge.rom_offset(0x4000), 0x134 * ROM_BANK);
    }

    #[test]
    fn the_cgb_flag_comes_from_the_header() {
        let mut rom = vec![0; 2 * ROM_BANK];
        rom[0x143] = 0x80;
        assert!(Cartridge::new(rom, None).supports_cgb());
        assert!(!cartridge(0x00, 2, 0).supports_cgb());
    }
}
