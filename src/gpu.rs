pub struct GPU {
    video_ram: [u8; 0x2000],
    pub bgp: u8,
    pub scy: u8
}

impl GPU {
    pub fn new() -> GPU {
        GPU {
            video_ram: [0; 0x2000],
            bgp: 0,
            scy: 0
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        self.video_ram[addr as usize]
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        self.video_ram[addr as usize] = value;
    }
}
