#[derive(PartialEq)]
pub struct Timer {
    counter: u16,
    tima: u8,
    tma: u8,
    tac: u8,
}

impl Timer {
    pub fn new() -> Timer {
        Timer {
            counter: 0xABCC,
            tima: 0,
            tma: 0,
            tac: 0,
        }
    }

    pub fn counter(&self) -> u16 {
        self.counter
    }

    fn enabled(&self) -> bool {
        self.tac & 0x04 != 0
    }

    fn tima_bit(&self) -> u16 {
        match self.tac & 0x03 {
            0 => 9,
            1 => 3,
            2 => 5,
            _ => 7,
        }
    }

    fn tima_signal(&self) -> bool {
        self.enabled() && (self.counter >> self.tima_bit()) & 1 != 0
    }

    fn increment_tima(&mut self, mut increments: usize) -> bool {
        let mut overflow = false;
        while increments > 0 {
            let room = 0x100 - self.tima as usize;
            if increments < room {
                self.tima += increments as u8;
                break;
            }
            increments -= room;
            self.tima = self.tma;
            overflow = true;
        }
        overflow
    }

    fn falling_edge(&mut self, update: impl FnOnce(&mut Timer)) -> bool {
        let before = self.tima_signal();
        update(self);
        before && !self.tima_signal() && self.increment_tima(1)
    }

    pub fn tick(&mut self, m_cycles: usize) -> bool {
        let t_cycles = m_cycles * 4;
        let start = self.counter as usize;
        self.counter = (start + t_cycles) as u16;
        if !self.enabled() {
            return false;
        }
        let period = 2usize << self.tima_bit();
        let edges = (start + t_cycles) / period - start / period;
        self.increment_tima(edges)
    }

    pub fn m_cycles_until_overflow(&self) -> usize {
        if !self.enabled() {
            return usize::MAX;
        }
        let period = 2usize << self.tima_bit();
        let first_edge = period - (self.counter as usize % period);
        let t_cycles = first_edge + (0xFF - self.tima as usize) * period;
        t_cycles.div_ceil(4)
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0xFF04 => (self.counter >> 8) as u8,
            0xFF05 => self.tima,
            0xFF06 => self.tma,
            _ => self.tac | 0xF8,
        }
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) -> bool {
        match addr {
            0xFF04 => self.falling_edge(|timer| timer.counter = 0),
            0xFF05 => {
                self.tima = value;
                false
            }
            0xFF06 => {
                self.tma = value;
                false
            }
            _ => self.falling_edge(|timer| timer.tac = value & 0x07),
        }
    }
}
