pub mod nullmbc;

pub trait MBC : Send {
    fn read_rom(&self, addr: u16) -> u8;
}
