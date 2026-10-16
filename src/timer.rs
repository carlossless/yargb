#[derive(Clone, Copy, PartialEq)]
enum Reload {
    Idle,
    Overflowed,
    Reloaded,
}

/// DIV, TIMA, TMA and TAC, ticked in T-cycles. The reload that follows a TIMA
/// overflow advances on M-cycle boundaries, tracked by `phase`.
#[derive(PartialEq)]
pub struct Timer {
    counter: u16,
    tima: u8,
    tma: u8,
    tac: u8,
    reload: Reload,
    phase: u8,
}

impl Timer {
    pub fn new(counter: u16) -> Timer {
        Timer {
            counter,
            tima: 0,
            tma: 0,
            tac: 0xF8,
            reload: Reload::Idle,
            phase: 0,
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

    fn increment_tima(&mut self) {
        let (tima, overflow) = self.tima.overflowing_add(1);
        self.tima = tima;
        if overflow {
            self.reload = Reload::Overflowed;
        }
    }

    fn set_counter(&mut self, counter: u16) {
        let before = self.tima_signal();
        self.counter = counter;
        if before && !self.tima_signal() {
            self.increment_tima();
        }
    }

    fn step_reload(&mut self) -> bool {
        match self.reload {
            Reload::Overflowed => {
                self.tima = self.tma;
                self.reload = Reload::Reloaded;
                true
            }
            Reload::Reloaded => {
                self.reload = Reload::Idle;
                false
            }
            Reload::Idle => false,
        }
    }

    /// Advances by T-cycles and returns whether the timer interrupt was raised.
    pub fn tick(&mut self, t_cycles: usize) -> bool {
        let mut interrupt = false;
        let mut remaining = t_cycles;
        while remaining > 0 && (self.enabled() || self.reload != Reload::Idle) {
            if self.phase == 0 {
                interrupt |= self.step_reload();
            }
            self.set_counter(self.counter.wrapping_add(1));
            self.phase = (self.phase + 1) % 4;
            remaining -= 1;
        }
        self.counter = self.counter.wrapping_add(remaining as u16);
        self.phase = ((self.phase as usize + remaining) % 4) as u8;
        interrupt
    }

    pub fn m_cycles_until_overflow(&self) -> usize {
        if self.reload == Reload::Overflowed {
            return 1;
        }
        if !self.enabled() {
            return usize::MAX;
        }
        let period = 2usize << self.tima_bit();
        let first_edge = period - (self.counter as usize % period);
        let t_cycles = first_edge + (0xFF - self.tima as usize) * period;
        t_cycles / 4
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0xFF04 => (self.counter >> 8) as u8,
            0xFF05 => self.tima,
            0xFF06 => self.tma,
            _ => self.tac,
        }
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            0xFF04 => self.set_counter(0),
            0xFF05 => match self.reload {
                Reload::Overflowed => {
                    self.tima = value;
                    self.reload = Reload::Idle;
                }
                Reload::Reloaded => {}
                Reload::Idle => self.tima = value,
            },
            0xFF06 => {
                self.tma = value;
                if self.reload == Reload::Reloaded {
                    self.tima = value;
                }
            }
            _ => {
                let before = self.tima_signal();
                self.tac = value | 0xF8;
                if before && !self.tima_signal() {
                    self.increment_tima();
                }
            }
        }
    }
}
