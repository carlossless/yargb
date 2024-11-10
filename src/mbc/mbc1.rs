use mbc::MBC;

pub struct MBC1 {
    rom: Vec<u8>,
    selected_rom_bank: u8,
}

impl MBC1 {
    pub fn new(data: Vec<u8>) -> MBC1 {
        MBC1 {
            rom: data,
            selected_rom_bank: 0,
        }
    }
}

impl MBC for MBC1 {
    fn read_rom(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x3FFF => self.rom[addr as usize],
            0x4000..=0x7FFF => {
                let bank = self.selected_rom_bank as usize;
                let addr = addr as usize - 0x4000;
                self.rom[bank * 0x4000 + addr]
            }
            _ => panic!("Invalid rom address {:#06X}", addr),
        }
    }

    fn write_rom(&mut self, addr: u16, value: u8) {
        match addr {
            0x2000..=0x3FFF => self.selected_rom_bank = value & 0x1F,
            _ => panic!("Invalid rom address {:#06X}", addr),
        }
    }
}
