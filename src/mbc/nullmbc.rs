use mbc::MBC;

pub struct NULLMBC {
    rom: Vec<u8>,
}

impl NULLMBC {
    pub fn new(data: Vec<u8>) -> NULLMBC {
        NULLMBC { rom: data }
    }
}

impl MBC for NULLMBC {
    fn read_rom(&self, addr: u16) -> u8 {
        self.rom[addr as usize]
    }
    fn write_rom(&mut self, addr: u16, _value: u8) {
        panic!(
            "Attempted to write to read-only memory at address {:#06X}",
            addr
        );
    }
}
