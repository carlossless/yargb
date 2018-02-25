use mbc::MBC;
use mbc::nullmbc::NULLMBC;

pub struct MMU {
    mbc: Box<MBC>,
    video_ram: [u8; 0x2000],
    work_ram: [u8; 0x2000],
    timer_stub: [u8; 0x0004],
    interupt_flags: u8,
    interupt_enable: u8,
    sound_flags: u8,
    sound_output: u8,
    channel_control: u8
}

impl MMU {
    pub fn new(rom_data: &[u8]) -> MMU {
        let mut rom: [u8; 32000] = [0; 32000];
        rom.copy_from_slice(&rom_data[..32000]);
        MMU {
            mbc: Box::new(NULLMBC::new(rom)),
            video_ram: [0; 0x2000],
            work_ram: [0; 0x2000],
            timer_stub: [0; 0x0004],
            interupt_flags: 0x00,
            interupt_enable: 0x00,
            sound_flags: 0xF1, //0xF0 for SGB
            sound_output: 0x00,
            channel_control: 0x00
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000 ... 0x7FFF => self.mbc.read_rom(addr),
            0x8000 ... 0x9FFF => self.video_ram[(addr - 0x8000) as usize],
            0xC000 ... 0xDFFF => self.work_ram[(addr - 0xC000) as usize],
            0xFF04 ... 0xFF07 => self.timer_stub[(addr - 0xFF04) as usize],
            0xFF0F => self.interupt_flags,
            0xFF24 => self.channel_control,
            0xFF25 => self.sound_output,
            0xFF26 => self.sound_flags,
            0xFF44 => { println!("lcdc_y_coordinate"); 0 }
            0xFFFF => self.interupt_enable,
            other => panic!("MMU for {:2X} is not implemented", other)
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        (self.read_byte(addr) as u16) | ((self.read_byte(addr + 1) as u16) << 8)
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            0x8000 ... 0x9FFF => { self.video_ram[(addr - 0x8000) as usize] = value; },
            0xC000 ... 0xDFFF => { self.work_ram[(addr - 0xC000) as usize] = value; },
            0xFF04 ... 0xFF07 => { self.timer_stub[(addr - 0xFF04) as usize] = value; },
            0xFF0F => { self.interupt_flags = value; println!("interupt_flags {:b}", value); }
            0xFF24 => { self.channel_control = value; println!("channel_control {:b}", value); }
            0xFF25 => { self.sound_output = value; println!("sound_output {:b}", value); }
            0xFF26 => { self.sound_flags = value; println!("sound_flags {:b}", value); }
            0xFF44 => { println!("lcdc_y_coordinate {:#2X}", value);  }
            0xFFFF => { self.interupt_enable = value; println!("interupt_enable {:b}", value); }
            other => panic!("MMU for {:2X} is not implemented", other)
        }
    }

    pub fn write_word(&mut self, addr: u16, value: u16) {
        self.write_byte(addr, (value & 0xFF) as u8);
        self.write_byte(addr + 1, (value >> 8) as u8);
    }

}
