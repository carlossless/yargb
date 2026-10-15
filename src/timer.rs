pub struct Timer {
    counter: u16,
    tima: u8,
    tma: u8,
    tac: u8,
}

impl Timer {
    pub fn new() -> Timer {
        Timer {
            counter: 0,
            tima: 0,
            tma: 0,
            tac: 0,
        }
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

    fn increment_tima(&mut self) -> bool {
        let (tima, overflow) = self.tima.overflowing_add(1);
        self.tima = if overflow { self.tma } else { tima };
        overflow
    }

    fn set_counter(&mut self, counter: u16) -> bool {
        let before = self.tima_signal();
        self.counter = counter;
        before && !self.tima_signal() && self.increment_tima()
    }

    pub fn tick(&mut self, m_cycles: usize) -> bool {
        let mut interrupt = false;
        for _ in 0..m_cycles {
            interrupt |= self.set_counter(self.counter.wrapping_add(4));
        }
        interrupt
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
            0xFF04 => self.set_counter(0),
            0xFF05 => {
                self.tima = value;
                false
            }
            0xFF06 => {
                self.tma = value;
                false
            }
            _ => {
                let before = self.tima_signal();
                self.tac = value & 0x07;
                before && !self.tima_signal() && self.increment_tima()
            }
        }
    }
}
