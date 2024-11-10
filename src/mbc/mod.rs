pub mod nullmbc;
pub mod mbc1;

pub trait MBC: Send {
    fn read_rom(&self, addr: u16) -> u8;
    fn write_rom(&mut self, addr: u16, value: u8);
}
