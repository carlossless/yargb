use mbc::MBC;
use mbc::nullmbc::NULLMBC;

pub struct MMU {
    mbc: Box<MBC>
}

impl MMU {
    pub fn new(rom_data: &[u8]) -> MMU {
        let mut rom: [u8; 32000] = [0; 32000];
        rom.copy_from_slice(&rom_data[..32000]);
        MMU {
            mbc: Box::new(NULLMBC::new(rom))
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000 ... 0x7D00 => self.mbc.read_rom(addr),
            other => panic!("MMU for {:2X} is not implemented", other)
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        (self.read_byte(addr) as u16) | ((self.read_byte(addr + 1) as u16) << 8)
    }

    pub fn write_byte(&self, addr: u16, value: u8) {
        match addr {
            other => panic!("MMU for {:2X} is not implemented", other)
        }
    }

    pub fn write_word(&self, addr: u16, value: u16) {
        self.write_byte(addr, (value & 0xFF) as u8);
        self.write_byte(addr + 1, (value >> 8) as u8);
    }

}
