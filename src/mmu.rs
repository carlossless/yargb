use gpu::GPU;
use mbc::nullmbc::NULLMBC;
use mbc::MBC;

use crate::mbc::mbc1::MBC1;

pub struct MMU {
    mbc: Box<dyn MBC>,
    gpu: GPU,
    work_ram: [u8; 0x2000],
    high_ram: [u8; 0x7F],
    timer_stub: [u8; 0x0004],
    interupt_flags: u8,
    interupt_enable: u8,
    sound_flags: u8,
    sound_output: u8,
    channel_control: u8,
    lcd_control: u8,
    unused_io: [u8; 0x0034],
}

impl MMU {
    pub fn new(rom_data: &Vec<u8>) -> MMU {
        let title = rom_data[0x134..0x143]
            .iter()
            .map(|&c| c as char)
            .collect::<String>();
        let manufacturer_code = rom_data[0x13F..0x143].to_owned();
        let cgb_flag = rom_data[0x143];
        let licensee_code_new = rom_data[0x144..0x146].to_owned();
        let sgb_flag = rom_data[0x146];
        let cartridge_type = rom_data[0x147];
        let rom_size = rom_data[0x148];
        let ram_size = rom_data[0x149];
        let destination_code = rom_data[0x14A];
        let licensee_code_old = rom_data[0x14B];
        let mask_rom_version = rom_data[0x14C];
        let header_checksum = rom_data[0x14D];
        let global_checksum = rom_data[0x14E..0x150]
            .iter()
            .map(|&c| c as u16)
            .sum::<u16>();

        // print!("Title: {}\nManufacturer Code: {:?}\n CGB Flag: {:02x}\n Licensee Code (new): {:?}\n SGB Flag: {:02x}\n Cartridge Type: {:02x}\n ROM Size: {:02x}\n RAM Size: {:02x}\n Destination Code: {:02x}\n Licensee Code (old): {:02x}\n Mask ROM Version: {:02x}\n Header Checksum: {:02x}\n Global Checksum: {:?}\n", title, manufacturer_code, cgb_flag, licensee_code_new, sgb_flag, cartridge_type, rom_size, ram_size, destination_code, licensee_code_old, mask_rom_version, header_checksum, global_checksum);

        let mbc: Box<dyn MBC> = match cartridge_type {
            0x00 => Box::new(NULLMBC::new(rom_data.to_owned())),
            0x01 => Box::new(MBC1::new(rom_data.to_owned())),
            _ => panic!("Cartridge Type {:02x} is unsupported", cartridge_type),
        };

        MMU {
            mbc: mbc,
            gpu: GPU::new(),
            work_ram: [0; 0x2000],
            high_ram: [0; 0x7F],
            timer_stub: [0; 0x0004],
            interupt_flags: 0x00,
            interupt_enable: 0x00,
            sound_flags: 0xF1, //0xF0 for SGB
            sound_output: 0x00,
            channel_control: 0x00,
            lcd_control: 0x00,
            unused_io: [0; 0x0034],
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => self.mbc.read_rom(addr),
            0x8000..=0x9FFF => self.gpu.read_byte(addr - 0x8000),
            0xC000..=0xDFFF => self.work_ram[(addr - 0xC000) as usize],
            0xFF00 => {
                println!("read joypad (unimplemented)");
                0x0f
            }
            0xFF01 => {
                println!("read serial_data");
                0x00
            }
            0xFF02 => {
                println!("read serial_clock");
                0x00
            }
            0xFF04..=0xFF07 => self.timer_stub[(addr - 0xFF04) as usize],
            0xFF0F => self.interupt_flags,
            0xFF24 => self.channel_control,
            0xFF25 => self.sound_output,
            0xFF26 => self.sound_flags,
            0xFF40 => self.lcd_control,
            0xFF42 => self.gpu.scy,
            0xFF43 => self.gpu.scx,
            0xFF44 => {
                /*println!("lcdc_y_coordinate");*/
                0x10
            } // TODO: needs to be implemented on GPU when drawing the line
            // 0xFF4C..=0xFF7F => self.unused_io[(addr - 0xFF4C) as usize], // TODO: has GPU functions
            0xFF4D => {
                println!("read speed_switch");
                return 0xff;
            }
            0xFF80..=0xFFFE => self.high_ram[(addr - 0xFF80) as usize],
            0xFFFF => self.interupt_enable,
            other => panic!("MMU read for {:2X} is not implemented", other),
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        (self.read_byte(addr) as u16) | ((self.read_byte(addr + 1) as u16) << 8)
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x7FFF => self.mbc.write_rom(addr, value),
            0x8000..=0x9FFF => self.gpu.write_byte(addr - 0x8000, value),
            0xC000..=0xDFFF => self.work_ram[(addr - 0xC000) as usize] = value,
            0xFF00 => {
                println!("write joypad (unimplemented)");
                println!("write joypad {:#2X}", value);
                if value & (1 << 4) != 0 {
                    println!("selected buttons: SsBA")
                } else {
                    println!("selected buttons: directional")
                }
            }
            0xFF01 => {
                println!("write serial_data {:#2X}", value);
                eprint!("{:}", value as char);
            }
            0xFF02 => println!("serial_clock {:#2X}", value),
            0xFF04..=0xFF07 => self.timer_stub[(addr - 0xFF04) as usize] = value,
            0xFF0F => {
                self.interupt_flags = value;
                println!("write interupt_flags {:b}", value);
            }
            0xFF24 => {
                self.channel_control = value;
                println!("write channel_control {:b}", value);
            }
            0xFF25 => {
                self.sound_output = value;
                println!("write sound_output {:b}", value);
            }
            0xFF26 => {
                self.sound_flags = value;
                println!("write sound_flags {:b}", value);
            }
            0xFF40 => {
                self.lcd_control = value;
            }
            0xFF42 => self.gpu.scy = value,
            0xFF43 => self.gpu.scx = value,
            0xFF44 => { /*println!("lcdc_y_coordinate {:#2X}", value);*/ } // TODO: needs to be implemented on GPU when drawing the line
            0xFF47 => self.gpu.bgp = value,
            0xFF68 => {
                println!("write bgp_index {:#2X}", value);
            }
            0xFF69 => {
                println!("write bgp_data {:#2X}", value);
            }
            0xFF6A => {
                println!("write obp_index {:#2X}", value);
            }
            0xFF6B => {
                println!("write obp_data {:#2X}", value);
            }

            0xFF4F => {
                println!("write vram_bank {:#2X}", value);
            }
            // 0xFF4C..=0xFF7F => {
            //     self.unused_io[(addr - 0xFF4C) as usize] = value;
            //     println!("write unused_io {:04x} {:02x}", addr, value);
            // }
            0xFF4D => {
                println!("write speed_switch {:#2X}", value);
            }
            0xFF80..=0xFFFE => self.high_ram[(addr - 0xFF80) as usize] = value,
            0xFFFF => {
                self.interupt_enable = value;
                println!("write interupt_enable {:b}", value);
            }
            other => panic!("MMU write for {:2X} is not implemented", other),
        }
    }

    pub fn write_word(&mut self, addr: u16, value: u16) {
        self.write_byte(addr, (value & 0xFF) as u8);
        self.write_byte(addr + 1, (value >> 8) as u8);
    }
}
