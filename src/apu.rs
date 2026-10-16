pub const SAMPLE_RATE: u32 = 48_000;
const CLOCK_RATE: u32 = 4_194_304;

const DUTY_PATTERNS: [[u8; 8]; 4] = [
    [0, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 0, 0, 0, 1, 1, 1],
    [0, 1, 1, 1, 1, 1, 1, 0],
];

const NOISE_DIVISORS: [u32; 8] = [8, 16, 32, 48, 64, 80, 96, 112];

/// Extra cycles before a triggered wave channel fetches its first sample.
const WAVE_TRIGGER_DELAY: u32 = 6;
/// The DMG reaches wave RAM during playback only this soon after a fetch.
const WAVE_ACCESS_WINDOW: u32 = 2;

const READ_MASKS: [u8; 0x17] = [
    0x80, 0x3F, 0x00, 0xFF, 0xBF, 0xFF, 0x3F, 0x00, 0xFF, 0xBF, 0x7F, 0xFF, 0x9F, 0xFF, 0xBF, 0xFF,
    0xFF, 0x00, 0x00, 0xBF, 0x00, 0x00, 0x70,
];

#[derive(Default, PartialEq)]
struct Length {
    counter: u16,
    enabled: bool,
}

#[derive(Default, PartialEq)]
struct Envelope {
    register: u8,
    volume: u8,
    timer: u8,
}

impl Envelope {
    fn dac_on(&self) -> bool {
        self.register & 0xF8 != 0
    }

    fn trigger(&mut self) {
        self.volume = self.register >> 4;
        self.timer = self.register & 7;
    }

    fn clock(&mut self) {
        let period = self.register & 7;
        if period == 0 {
            return;
        }
        self.timer = self.timer.saturating_sub(1);
        if self.timer == 0 {
            self.timer = period;
            if self.register & 0x08 != 0 && self.volume < 15 {
                self.volume += 1;
            } else if self.register & 0x08 == 0 && self.volume > 0 {
                self.volume -= 1;
            }
        }
    }
}

#[derive(Default, PartialEq)]
struct Sweep {
    register: u8,
    timer: u8,
    enabled: bool,
    shadow: u16,
    negated: bool,
}

impl Sweep {
    fn period(&self) -> u8 {
        (self.register >> 4) & 7
    }

    fn shift(&self) -> u8 {
        self.register & 7
    }

    fn next_frequency(&mut self) -> u16 {
        let delta = self.shadow >> self.shift();
        if self.register & 0x08 != 0 {
            self.negated = true;
            self.shadow - delta
        } else {
            self.shadow + delta
        }
    }
}

#[derive(Default, PartialEq)]
struct Square {
    enabled: bool,
    length: Length,
    envelope: Envelope,
    duty: u8,
    frequency: u16,
    timer: u32,
    position: u8,
}

impl Square {
    fn period(&self) -> u32 {
        (2048 - self.frequency as u32) * 4
    }

    fn advance(&mut self, mut cycles: u32) {
        while cycles >= self.timer {
            cycles -= self.timer;
            self.timer = self.period();
            self.position = (self.position + 1) % 8;
        }
        self.timer -= cycles;
    }

    fn output(&self) -> Option<u8> {
        self.envelope.dac_on().then(|| {
            let high = DUTY_PATTERNS[self.duty as usize][self.position as usize];
            if self.enabled {
                high * self.envelope.volume
            } else {
                0
            }
        })
    }
}

#[derive(PartialEq)]
struct Wave {
    enabled: bool,
    dac_on: bool,
    length: Length,
    volume: u8,
    frequency: u16,
    timer: u32,
    position: u8,
    since_fetch: u32,
    ram: [u8; 16],
}

impl Wave {
    fn period(&self) -> u32 {
        (2048 - self.frequency as u32) * 2
    }

    fn advance(&mut self, mut cycles: u32) {
        while cycles >= self.timer {
            cycles -= self.timer;
            self.timer = self.period();
            self.position = (self.position + 1) % 32;
            self.since_fetch = 0;
        }
        self.timer -= cycles;
        self.since_fetch = self.since_fetch.saturating_add(cycles);
    }

    /// Retriggering the DMG wave channel just as it fetches overwrites the
    /// start of wave RAM with the bytes it was about to read.
    fn corrupt_on_retrigger(&mut self) {
        let offset = (self.position as usize).div_ceil(2) % 16;
        if offset < 4 {
            self.ram[0] = self.ram[offset];
        } else {
            let start = offset & !3;
            self.ram.copy_within(start..start + 4, 0);
        }
    }

    fn ram_index(&self, addr: u16, cgb: bool) -> Option<usize> {
        if !self.enabled {
            Some(addr as usize - 0xFF30)
        } else if cgb || self.since_fetch < WAVE_ACCESS_WINDOW {
            Some(self.position as usize / 2)
        } else {
            None
        }
    }

    fn output(&self) -> Option<u8> {
        self.dac_on.then(|| {
            let byte = self.ram[self.position as usize / 2];
            let sample = if self.position.is_multiple_of(2) {
                byte >> 4
            } else {
                byte & 0x0F
            };
            match (self.enabled, self.volume) {
                (false, _) | (_, 0) => 0,
                (_, shift) => sample >> (shift - 1),
            }
        })
    }
}

#[derive(Default, PartialEq)]
struct Noise {
    enabled: bool,
    length: Length,
    envelope: Envelope,
    register: u8,
    lfsr: u16,
    timer: u32,
}

impl Noise {
    fn period(&self) -> u32 {
        NOISE_DIVISORS[self.register as usize & 7] << (self.register >> 4)
    }

    fn advance(&mut self, mut cycles: u32) {
        if self.register >> 4 >= 14 {
            return;
        }
        while cycles >= self.timer {
            cycles -= self.timer;
            self.timer = self.period();
            let feedback = (self.lfsr ^ (self.lfsr >> 1)) & 1;
            self.lfsr = (self.lfsr >> 1) | feedback << 14;
            if self.register & 0x08 != 0 {
                self.lfsr = self.lfsr & !0x40 | feedback << 6;
            }
        }
        self.timer -= cycles;
    }

    fn output(&self) -> Option<u8> {
        self.envelope.dac_on().then(|| {
            if self.enabled {
                (!self.lfsr & 1) as u8 * self.envelope.volume
            } else {
                0
            }
        })
    }
}

/// The audio processing unit. Channels are mixed into interleaved stereo
/// samples at `SAMPLE_RATE`, which the frontend drains with `take_samples`.
#[derive(PartialEq)]
pub struct Apu {
    cgb: bool,
    powered: bool,
    registers: [u8; 0x17],
    square1: Square,
    sweep: Sweep,
    square2: Square,
    wave: Wave,
    noise: Noise,
    frame_step: u8,
    sample_phase: u32,
    capacitors: [f32; 2],
    samples: Vec<f32>,
}

impl Apu {
    pub fn new(cgb: bool) -> Apu {
        let mut apu = Apu {
            cgb,
            powered: true,
            registers: [0; 0x17],
            square1: Square::default(),
            sweep: Sweep::default(),
            square2: Square::default(),
            wave: Wave {
                enabled: false,
                dac_on: false,
                length: Length::default(),
                volume: 0,
                frequency: 0,
                timer: 0,
                position: 0,
                since_fetch: u32::MAX,
                ram: [0; 16],
            },
            noise: Noise::default(),
            frame_step: 0,
            sample_phase: 0,
            capacitors: [0.0; 2],
            samples: Vec::new(),
        };
        for (addr, value) in [
            (0xFF10, 0x80),
            (0xFF11, 0xBF),
            (0xFF12, 0xF3),
            (0xFF14, 0x3F),
            (0xFF16, 0x3F),
            (0xFF19, 0x3F),
            (0xFF1A, 0x7F),
            (0xFF1B, 0xFF),
            (0xFF1C, 0x9F),
            (0xFF1E, 0x3F),
            (0xFF20, 0xFF),
            (0xFF23, 0x3F),
            (0xFF24, 0x77),
            (0xFF25, 0xF3),
        ] {
            apu.write(addr, value);
        }
        apu.square1.enabled = true;
        apu
    }

    pub fn take_samples(&mut self) -> Vec<f32> {
        std::mem::take(&mut self.samples)
    }

    fn length_steps_next(&self) -> bool {
        self.frame_step.is_multiple_of(2)
    }

    pub fn step_frame_sequencer(&mut self) {
        if !self.powered {
            return;
        }
        let step = self.frame_step;
        self.frame_step = (step + 1) % 8;
        if step.is_multiple_of(2) {
            for (length, enabled) in [
                (&mut self.square1.length, &mut self.square1.enabled),
                (&mut self.square2.length, &mut self.square2.enabled),
                (&mut self.wave.length, &mut self.wave.enabled),
                (&mut self.noise.length, &mut self.noise.enabled),
            ] {
                if length.enabled && length.counter > 0 {
                    length.counter -= 1;
                    if length.counter == 0 {
                        *enabled = false;
                    }
                }
            }
        }
        if step == 2 || step == 6 {
            self.clock_sweep();
        }
        if step == 7 {
            self.square1.envelope.clock();
            self.square2.envelope.clock();
            self.noise.envelope.clock();
        }
    }

    fn clock_sweep(&mut self) {
        self.sweep.timer = self.sweep.timer.saturating_sub(1);
        if self.sweep.timer != 0 {
            return;
        }
        self.sweep.timer = match self.sweep.period() {
            0 => 8,
            period => period,
        };
        if !self.sweep.enabled || self.sweep.period() == 0 {
            return;
        }
        let frequency = self.sweep.next_frequency();
        if frequency > 2047 {
            self.square1.enabled = false;
        } else if self.sweep.shift() != 0 {
            self.sweep.shadow = frequency;
            self.square1.frequency = frequency;
            if self.sweep.next_frequency() > 2047 {
                self.square1.enabled = false;
            }
        }
    }

    pub fn tick(&mut self, t_cycles: usize) {
        let mut remaining = t_cycles as u32;
        while remaining > 0 {
            let until_sample = (CLOCK_RATE - self.sample_phase).div_ceil(SAMPLE_RATE);
            let step = remaining.min(until_sample);
            remaining -= step;
            if self.powered {
                self.square1.advance(step);
                self.square2.advance(step);
                self.wave.advance(step);
                self.noise.advance(step);
            }
            self.sample_phase += step * SAMPLE_RATE;
            if self.sample_phase >= CLOCK_RATE {
                self.sample_phase -= CLOCK_RATE;
                self.mix();
            }
        }
    }

    fn mix(&mut self) {
        let outputs = [
            self.square1.output(),
            self.square2.output(),
            self.wave.output(),
            self.noise.output(),
        ];
        let panning = self.registers[0x15];
        let volumes = self.registers[0x14];
        for (side, shift) in [(0, 4), (1, 0)] {
            let mut sum = 0.0;
            for (channel, output) in outputs.iter().enumerate() {
                if let Some(digital) = output
                    && panning & (1 << (channel + shift)) != 0
                {
                    sum += *digital as f32 / 7.5 - 1.0;
                }
            }
            let volume = ((volumes >> shift) & 7) as f32 + 1.0;
            let input = if self.powered {
                sum * volume / 32.0
            } else {
                0.0
            };
            let output = input - self.capacitors[side];
            self.capacitors[side] = input - output * 0.996;
            self.samples.push(output);
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0xFF26 => {
                0x70 | (self.powered as u8) << 7
                    | self.square1.enabled as u8
                    | (self.square2.enabled as u8) << 1
                    | (self.wave.enabled as u8) << 2
                    | (self.noise.enabled as u8) << 3
            }
            0xFF10..=0xFF25 => {
                let index = addr as usize - 0xFF10;
                self.registers[index] | READ_MASKS[index]
            }
            0xFF30..=0xFF3F => self
                .wave
                .ram_index(addr, self.cgb)
                .map_or(0xFF, |index| self.wave.ram[index]),
            _ => 0xFF,
        }
    }

    /// The digital outputs of the two square channels, as the CGB's PCM12 register.
    pub fn pcm12(&self) -> u8 {
        let level = |output: Option<u8>| output.unwrap_or(0);
        level(self.square1.output()) | level(self.square2.output()) << 4
    }

    /// The digital outputs of the wave and noise channels, as PCM34.
    pub fn pcm34(&self) -> u8 {
        let level = |output: Option<u8>| output.unwrap_or(0);
        level(self.wave.output()) | level(self.noise.output()) << 4
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        if addr == 0xFF26 {
            let powered = value & 0x80 != 0;
            if self.powered && !powered {
                for addr in 0xFF10..=0xFF25 {
                    self.write(addr, 0);
                }
                self.square1.enabled = false;
                self.square2.enabled = false;
                self.wave.enabled = false;
                self.noise.enabled = false;
                if self.cgb {
                    self.square1.length.counter = 0;
                    self.square2.length.counter = 0;
                    self.wave.length.counter = 0;
                    self.noise.length.counter = 0;
                }
            } else if !self.powered && powered {
                self.frame_step = 0;
                self.square1.position = 0;
                self.square2.position = 0;
                self.wave.position = 0;
            }
            self.powered = powered;
            return;
        }
        if (0xFF30..=0xFF3F).contains(&addr) {
            if let Some(index) = self.wave.ram_index(addr, self.cgb) {
                self.wave.ram[index] = value;
            }
            return;
        }
        if !(0xFF10..=0xFF25).contains(&addr) {
            return;
        }
        if !self.powered && self.cgb {
            return;
        }
        if !self.powered {
            match addr {
                0xFF11 | 0xFF16 => {}
                0xFF1B => self.wave.length.counter = 256 - value as u16,
                0xFF20 => self.noise.length.counter = 64 - (value & 0x3F) as u16,
                _ => return,
            }
            if addr == 0xFF11 {
                self.square1.length.counter = 64 - (value & 0x3F) as u16;
            } else if addr == 0xFF16 {
                self.square2.length.counter = 64 - (value & 0x3F) as u16;
            }
            return;
        }
        self.registers[addr as usize - 0xFF10] = value;
        let length_steps_next = self.length_steps_next();
        match addr {
            0xFF10 => {
                let was_negate = self.sweep.register & 0x08 != 0;
                self.sweep.register = value;
                if was_negate && value & 0x08 == 0 && self.sweep.negated {
                    self.square1.enabled = false;
                }
            }
            0xFF11 | 0xFF16 => {
                let square = if addr == 0xFF11 {
                    &mut self.square1
                } else {
                    &mut self.square2
                };
                square.duty = value >> 6;
                square.length.counter = 64 - (value & 0x3F) as u16;
            }
            0xFF12 | 0xFF17 => {
                let square = if addr == 0xFF12 {
                    &mut self.square1
                } else {
                    &mut self.square2
                };
                square.envelope.register = value;
                if !square.envelope.dac_on() {
                    square.enabled = false;
                }
            }
            0xFF13 | 0xFF18 => {
                let square = if addr == 0xFF13 {
                    &mut self.square1
                } else {
                    &mut self.square2
                };
                square.frequency = square.frequency & 0x700 | value as u16;
            }
            0xFF14 | 0xFF19 => {
                let first = addr == 0xFF14;
                let square = if first {
                    &mut self.square1
                } else {
                    &mut self.square2
                };
                square.frequency = square.frequency & 0xFF | (value as u16 & 7) << 8;
                let triggered = write_length_control(
                    &mut square.length,
                    &mut square.enabled,
                    value,
                    64,
                    length_steps_next,
                );
                if triggered {
                    square.enabled = square.envelope.dac_on();
                    square.timer = square.period();
                    square.envelope.trigger();
                    if first {
                        self.trigger_sweep();
                    }
                }
            }
            0xFF1A => {
                self.wave.dac_on = value & 0x80 != 0;
                if !self.wave.dac_on {
                    self.wave.enabled = false;
                }
            }
            0xFF1B => self.wave.length.counter = 256 - value as u16,
            0xFF1C => self.wave.volume = (value >> 5) & 3,
            0xFF1D => self.wave.frequency = self.wave.frequency & 0x700 | value as u16,
            0xFF1E => {
                self.wave.frequency = self.wave.frequency & 0xFF | (value as u16 & 7) << 8;
                let wave = &mut self.wave;
                let was_playing = wave.enabled;
                if write_length_control(
                    &mut wave.length,
                    &mut wave.enabled,
                    value,
                    256,
                    length_steps_next,
                ) {
                    if !self.cgb && was_playing && wave.timer <= WAVE_ACCESS_WINDOW {
                        wave.corrupt_on_retrigger();
                    }
                    wave.enabled = wave.dac_on;
                    wave.timer = wave.period() + WAVE_TRIGGER_DELAY;
                    wave.position = 0;
                }
            }
            0xFF20 => self.noise.length.counter = 64 - (value & 0x3F) as u16,
            0xFF21 => {
                self.noise.envelope.register = value;
                if !self.noise.envelope.dac_on() {
                    self.noise.enabled = false;
                }
            }
            0xFF22 => self.noise.register = value,
            0xFF23 => {
                let noise = &mut self.noise;
                if write_length_control(
                    &mut noise.length,
                    &mut noise.enabled,
                    value,
                    64,
                    length_steps_next,
                ) {
                    noise.enabled = noise.envelope.dac_on();
                    noise.timer = noise.period();
                    noise.lfsr = 0x7FFF;
                    noise.envelope.trigger();
                }
            }
            _ => {}
        }
    }

    fn trigger_sweep(&mut self) {
        self.sweep.shadow = self.square1.frequency;
        self.sweep.timer = match self.sweep.period() {
            0 => 8,
            period => period,
        };
        self.sweep.enabled = self.sweep.period() != 0 || self.sweep.shift() != 0;
        self.sweep.negated = false;
        if self.sweep.shift() != 0 && self.sweep.next_frequency() > 2047 {
            self.square1.enabled = false;
        }
    }
}

/// Applies an NRx4 write's length enable and trigger bits, including the extra
/// length clock that hardware applies when the frame sequencer's next step
/// will not clock length. Returns whether the channel was triggered.
fn write_length_control(
    length: &mut Length,
    enabled: &mut bool,
    value: u8,
    max: u16,
    length_steps_next: bool,
) -> bool {
    let was_enabled = length.enabled;
    length.enabled = value & 0x40 != 0;
    let triggered = value & 0x80 != 0;
    let extra_clock = !length_steps_next && length.enabled;
    if extra_clock && !was_enabled && length.counter > 0 {
        length.counter -= 1;
        if length.counter == 0 && !triggered {
            *enabled = false;
        }
    }
    if triggered && length.counter == 0 {
        length.counter = if extra_clock { max - 1 } else { max };
    }
    triggered
}

#[cfg(test)]
mod tests {
    use super::*;

    const NR52: u16 = 0xFF26;

    fn channels_on(apu: &Apu) -> u8 {
        apu.read(NR52) & 0x0F
    }

    /// Starts the wave channel at its slowest rate with distinct bytes in wave RAM.
    fn playing_wave(cgb: bool) -> Apu {
        let mut apu = Apu::new(cgb);
        for (index, addr) in (0xFF30..=0xFF3F).enumerate() {
            apu.write(addr, index as u8 * 0x11);
        }
        apu.write(0xFF1A, 0x80);
        apu.write(0xFF1D, 0x00);
        apu.write(0xFF1E, 0x80);
        apu
    }

    #[test]
    fn unused_register_bits_read_as_set() {
        let mut apu = Apu::new(false);
        apu.write(0xFF10, 0);
        apu.write(0xFF1A, 0);
        assert_eq!(apu.read(0xFF10), 0x80);
        assert_eq!(apu.read(0xFF1A), 0x7F);
        assert_eq!(apu.read(0xFF13), 0xFF);
        assert_eq!(apu.read(0xFF27), 0xFF);
    }

    #[test]
    fn powering_off_clears_registers_and_channels() {
        let mut apu = Apu::new(false);
        assert_eq!(channels_on(&apu), 0b0001);
        apu.write(NR52, 0);
        assert_eq!(apu.read(NR52), 0x70);
        assert_eq!(apu.read(0xFF12), 0x00);
        apu.write(0xFF12, 0xF0);
        assert_eq!(apu.read(0xFF12), 0x00, "writes are ignored while off");
        apu.write(NR52, 0x80);
        assert_eq!(apu.read(NR52), 0xF0);
    }

    #[test]
    fn the_dmg_keeps_length_writes_while_powered_off() {
        let mut dmg = Apu::new(false);
        dmg.write(NR52, 0);
        dmg.write(0xFF11, 0x3F);
        dmg.write(NR52, 0x80);
        dmg.write(0xFF12, 0xF0);
        dmg.write(0xFF14, 0xC0);
        assert_eq!(channels_on(&dmg), 0b0001);
        dmg.step_frame_sequencer();
        assert_eq!(channels_on(&dmg), 0, "length 1 expires on the first clock");
    }

    #[test]
    fn triggering_needs_the_dac_on() {
        let mut apu = Apu::new(false);
        apu.write(0xFF17, 0x00);
        apu.write(0xFF19, 0x80);
        assert_eq!(channels_on(&apu) & 0b0010, 0);
        apu.write(0xFF17, 0xF0);
        apu.write(0xFF19, 0x80);
        assert_eq!(channels_on(&apu) & 0b0010, 0b0010);
        apu.write(0xFF17, 0x00);
        assert_eq!(
            channels_on(&apu) & 0b0010,
            0,
            "turning the DAC off stops the channel"
        );
    }

    #[test]
    fn length_counters_stop_their_channels() {
        let mut apu = Apu::new(false);
        apu.write(0xFF21, 0xF0);
        apu.write(0xFF20, 64 - 3);
        apu.write(0xFF23, 0xC0);
        assert_eq!(channels_on(&apu) & 0b1000, 0b1000);
        for _ in 0..6 {
            apu.step_frame_sequencer();
        }
        assert_eq!(channels_on(&apu) & 0b1000, 0);
    }

    #[test]
    fn sweep_overflow_on_trigger_disables_square_1() {
        let mut apu = Apu::new(false);
        apu.write(0xFF10, 0x11);
        apu.write(0xFF12, 0xF0);
        apu.write(0xFF13, 0xFF);
        apu.write(0xFF14, 0x87);
        assert_eq!(channels_on(&apu) & 0b0001, 0);
    }

    #[test]
    fn samples_come_out_at_the_output_rate() {
        let mut apu = Apu::new(false);
        apu.tick(CLOCK_RATE as usize);
        let frames = apu.take_samples().len() / 2;
        assert!((SAMPLE_RATE as usize - 1..=SAMPLE_RATE as usize).contains(&frames));
    }

    #[test]
    fn the_dmg_reads_wave_ram_while_playing_only_on_a_fetch() {
        let mut apu = playing_wave(false);
        assert_eq!(apu.read(0xFF30), 0xFF);
        apu.tick((2048 * 2 + WAVE_TRIGGER_DELAY) as usize);
        assert_eq!(
            apu.read(0xFF35),
            0x00,
            "the byte being played, whatever the address"
        );
        apu.tick(WAVE_ACCESS_WINDOW as usize);
        assert_eq!(apu.read(0xFF35), 0xFF);
    }

    #[test]
    fn the_cgb_reads_the_playing_wave_byte() {
        let mut apu = playing_wave(true);
        apu.tick((2048 * 2 * 3 + WAVE_TRIGGER_DELAY) as usize);
        assert_eq!(apu.read(0xFF3F), 0x11);
    }

    #[test]
    fn wave_ram_is_plain_memory_while_stopped() {
        let mut apu = Apu::new(false);
        apu.write(0xFF35, 0x5A);
        assert_eq!(apu.read(0xFF35), 0x5A);
    }

    #[test]
    fn retriggering_the_dmg_wave_channel_as_it_fetches_corrupts_wave_ram() {
        let mut apu = playing_wave(false);
        let fetch = 2048 * 2;
        apu.tick((fetch * 8 + WAVE_TRIGGER_DELAY) as usize - 1);
        apu.write(0xFF1E, 0x80);
        apu.write(0xFF1A, 0x00);
        let ram: Vec<u8> = (0xFF30..=0xFF3F).map(|addr| apu.read(addr)).collect();
        assert_eq!(ram[..4], [0x44, 0x55, 0x66, 0x77]);
        assert_eq!(ram[4..], (4..16).map(|i| i * 0x11).collect::<Vec<u8>>()[..]);
    }
}
