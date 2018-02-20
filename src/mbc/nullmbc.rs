use mbc::MBC;

const ROM_SIZE: usize = 0x7D00; // 32kB

pub struct NULLMBC {
    rom: [u8; ROM_SIZE] // 32kB
}

impl NULLMBC {
    pub fn new(data: [u8; ROM_SIZE]) -> NULLMBC {
        NULLMBC { rom: data }
    }
}

impl MBC for NULLMBC {
    fn read_rom(&self, addr: u16) -> u8 { self.rom[addr as usize] }
}
