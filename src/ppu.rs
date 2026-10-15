pub const SCREEN_WIDTH: usize = 160;
pub const SCREEN_HEIGHT: usize = 144;

const LINE_DOTS: u32 = 456;
const OAM_SCAN_DOTS: u32 = 80;
const MIN_TRANSFER_DOTS: u32 = 172;
const LINES: u8 = 154;
const FRAME_DOTS: u32 = LINE_DOTS * LINES as u32;
const MAX_LINE_SPRITES: usize = 10;
/// Dots the background fetcher must have spent on its current tile before an
/// object fetch can take over.
const SPRITE_FETCH_WAIT: u8 = 4;
/// Dots before STAT and the VRAM/OAM locks reflect a switch to mode 3 or mode 0,
/// counting the dot of the switch. Interrupts see the switch immediately.
const VISIBLE_MODE_LAG: u8 = 2;
/// Turning the LCD on drops the PPU into line 0 this many dots in.
const LCD_ON_DOT: u32 = 3;
/// Line 0 after turning the LCD on has no OAM scan and starts its transfer
/// 2 dots later than other lines.
const FIRST_LINE_TRANSFER_DOT: u32 = OAM_SCAN_DOTS + 2;

pub const VBLANK_INTERRUPT: u8 = 1 << 0;
pub const STAT_INTERRUPT: u8 = 1 << 1;

const MODE_HBLANK: u8 = 0;
const MODE_VBLANK: u8 = 1;
const MODE_OAM_SCAN: u8 = 2;
const MODE_TRANSFER: u8 = 3;

const STAT_HBLANK: u8 = 1 << 3;
const STAT_VBLANK: u8 = 1 << 4;
const STAT_OAM: u8 = 1 << 5;
const STAT_LYC: u8 = 1 << 6;

#[derive(Clone, Copy, Default, PartialEq)]
struct Sprite {
    x: u8,
    y: u8,
    oam_index: u8,
    fetched: bool,
}

#[derive(Clone, Copy, Default, PartialEq)]
struct ObjPixel {
    color: u8,
    palette: u8,
    behind_bg: bool,
}

#[derive(Clone, Copy, PartialEq)]
enum FetchStep {
    Tile,
    Low,
    High,
    Push,
}

#[derive(Clone, Copy, PartialEq)]
struct Fetcher {
    step: FetchStep,
    dot: u8,
    column: u8,
    tile: u8,
    low: u8,
    high: u8,
    window: bool,
    warmup: bool,
    since_push: u8,
}

impl Fetcher {
    /// The warm-up fetch at the start of mode 3 takes 4 dots and its 8 pixels
    /// are shifted out unseen, though objects can still be matched against them.
    fn new(window: bool, warmup: bool) -> Fetcher {
        Fetcher {
            since_push: 0,
            step: if warmup {
                FetchStep::Low
            } else {
                FetchStep::Tile
            },
            dot: 0,
            column: 0,
            tile: 0,
            low: 0,
            high: 0,
            window,
            warmup,
        }
    }
}

/// The picture processing unit, emulated dot by dot. Pixels come out of a
/// background FIFO fed by a tile fetcher, mixed with an object FIFO, so the
/// length of mode 3 and the effect of mid-line register writes follow from the
/// fetches the hardware makes.
#[derive(PartialEq)]
pub struct Ppu {
    vram: [u8; 0x2000],
    oam: [u8; 0xA0],
    lcdc: u8,
    stat: u8,
    scy: u8,
    scx: u8,
    ly: u8,
    lyc: u8,
    bgp: u8,
    obp0: u8,
    obp1: u8,
    wy: u8,
    wx: u8,
    mode: u8,
    visible_mode: u8,
    visible_lag: u8,
    dot: u32,
    stat_line: bool,
    lyc_match: bool,
    sprites: [Sprite; MAX_LINE_SPRITES],
    sprite_count: usize,
    fetcher: Fetcher,
    bg_fifo: [u8; 8],
    bg_len: u8,
    obj_fifo: [ObjPixel; 8],
    obj_len: u8,
    lcd_x: i16,
    discard: u8,
    sprite_fetch: Option<(usize, u8)>,
    window_line: u8,
    window_drawn: bool,
    window_y_hit: bool,
    first_line: bool,
    frame: Box<[u8; SCREEN_WIDTH * SCREEN_HEIGHT]>,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            vram: [0; 0x2000],
            oam: [0; 0xA0],
            lcdc: 0x91,
            stat: 0,
            scy: 0,
            scx: 0,
            ly: 0,
            lyc: 0,
            bgp: 0xFC,
            obp0: 0xFF,
            obp1: 0xFF,
            wy: 0,
            wx: 0,
            mode: MODE_OAM_SCAN,
            visible_mode: MODE_OAM_SCAN,
            visible_lag: 0,
            dot: 0,
            stat_line: false,
            lyc_match: true,
            sprites: [Sprite::default(); MAX_LINE_SPRITES],
            sprite_count: 0,
            fetcher: Fetcher::new(false, true),
            bg_fifo: [0; 8],
            bg_len: 0,
            obj_fifo: [ObjPixel::default(); 8],
            obj_len: 0,
            lcd_x: 0,
            discard: 0,
            sprite_fetch: None,
            window_line: 0,
            window_drawn: false,
            window_y_hit: false,
            first_line: false,
            frame: Box::new([0; SCREEN_WIDTH * SCREEN_HEIGHT]),
        }
    }

    pub fn frame(&self) -> &[u8; SCREEN_WIDTH * SCREEN_HEIGHT] {
        &self.frame
    }

    fn enabled(&self) -> bool {
        self.lcdc & 0x80 != 0
    }

    pub fn vram_accessible(&self) -> bool {
        self.mode != MODE_TRANSFER && self.visible_mode != MODE_TRANSFER
    }

    pub fn oam_readable(&self) -> bool {
        self.mode != MODE_OAM_SCAN
            && self.visible_mode != MODE_TRANSFER
            && self.visible_mode != MODE_OAM_SCAN
    }

    /// The OAM scan stops locking out writes for its last 4 dots.
    pub fn oam_writable(&self) -> bool {
        let scanning = self.mode == MODE_OAM_SCAN && self.dot < OAM_SCAN_DOTS - 4;
        !scanning && self.mode != MODE_TRANSFER && self.visible_mode != MODE_TRANSFER
    }

    fn update_visible_mode(&mut self) {
        if self.visible_mode == self.mode {
            self.visible_lag = 0;
            return;
        }
        if self.mode == MODE_VBLANK {
            self.visible_mode = self.mode;
            return;
        }
        if self.visible_lag == 0 {
            self.visible_lag = VISIBLE_MODE_LAG;
        }
        self.visible_lag -= 1;
        if self.visible_lag == 0 {
            self.visible_mode = self.mode;
        }
    }

    fn update_stat_line(&mut self) -> u8 {
        let mode = self.mode;
        let line = self.stat & STAT_LYC != 0 && self.lyc_match
            || self.enabled()
                && (self.stat & STAT_HBLANK != 0 && mode == MODE_HBLANK
                    || self.stat & STAT_VBLANK != 0 && mode == MODE_VBLANK
                    || self.stat & STAT_OAM != 0
                        && (mode == MODE_OAM_SCAN
                            || self.ly == SCREEN_HEIGHT as u8 && self.dot == 0));
        let rising = line && !self.stat_line;
        self.stat_line = line;
        if rising { STAT_INTERRUPT } else { 0 }
    }

    pub fn tick(&mut self, t_cycles: usize) -> u8 {
        if !self.enabled() {
            return 0;
        }
        let mut interrupts = 0;
        for _ in 0..t_cycles {
            interrupts |= self.step();
        }
        interrupts
    }

    fn step(&mut self) -> u8 {
        let mut interrupts = 0;
        match self.mode {
            MODE_OAM_SCAN => {
                if self.dot % 2 == 0 && self.dot < OAM_SCAN_DOTS {
                    self.scan_oam_entry((self.dot / 2) as usize);
                }
                if self.dot + 1 == OAM_SCAN_DOTS {
                    self.start_transfer();
                }
            }
            MODE_TRANSFER => {
                self.transfer_dot();
                if self.lcd_x == SCREEN_WIDTH as i16 {
                    self.mode = MODE_HBLANK;
                }
            }
            _ => {}
        }
        self.dot += 1;
        if self.first_line && self.dot == FIRST_LINE_TRANSFER_DOT && self.ly == 0 {
            self.start_transfer();
        }
        if self.dot == LINE_DOTS {
            interrupts |= self.next_line();
        }
        self.update_visible_mode();
        self.lyc_match = self.dot != 0 && self.ly == self.lyc;
        interrupts | self.update_stat_line()
    }

    fn next_line(&mut self) -> u8 {
        self.dot = 0;
        self.first_line = false;
        if self.window_drawn {
            self.window_line += 1;
            self.window_drawn = false;
        }
        self.ly = (self.ly + 1) % LINES;
        if self.ly == 0 {
            self.window_line = 0;
            self.window_y_hit = false;
        }
        if self.ly < SCREEN_HEIGHT as u8 {
            self.mode = MODE_OAM_SCAN;
            self.sprite_count = 0;
            if self.ly == self.wy {
                self.window_y_hit = true;
            }
            0
        } else if self.ly == SCREEN_HEIGHT as u8 {
            self.mode = MODE_VBLANK;
            VBLANK_INTERRUPT
        } else {
            0
        }
    }

    fn scan_oam_entry(&mut self, index: usize) {
        if index == 0 {
            self.sprite_count = 0;
            if self.ly == self.wy {
                self.window_y_hit = true;
            }
        }
        if self.sprite_count == MAX_LINE_SPRITES {
            return;
        }
        let y = self.oam[index * 4];
        let height = if self.lcdc & 0x04 != 0 { 16 } else { 8 };
        let top = y as i32 - 16;
        if (top..top + height).contains(&(self.ly as i32)) {
            self.sprites[self.sprite_count] = Sprite {
                x: self.oam[index * 4 + 1],
                y,
                oam_index: index as u8,
                fetched: false,
            };
            self.sprite_count += 1;
        }
    }

    fn start_transfer(&mut self) {
        self.mode = MODE_TRANSFER;
        self.fetcher = Fetcher::new(false, true);
        self.bg_len = 0;
        self.obj_len = 0;
        self.lcd_x = -8;
        self.discard = self.scx % 8;
        self.sprite_fetch = None;
    }

    fn transfer_dot(&mut self) {
        if self.sprite_fetch.is_none() && self.lcdc & 0x02 != 0 {
            let x = self.lcd_x + 8;
            self.sprite_fetch = self.sprites[..self.sprite_count]
                .iter()
                .position(|sprite| !sprite.fetched && sprite.x as i16 <= x)
                .map(|index| (index, 0));
        }
        if let Some((index, dots)) = self.sprite_fetch {
            if self.bg_len == 0 || self.fetcher.since_push < SPRITE_FETCH_WAIT {
                self.advance_fetcher();
                return;
            }
            if dots + 1 < 6 {
                self.sprite_fetch = Some((index, dots + 1));
                return;
            }
            self.fetch_sprite(index);
            self.sprite_fetch = None;
            return;
        }

        self.advance_fetcher();
        if self.bg_len == 0 {
            return;
        }
        if !self.fetcher.window && self.lcd_x >= 0 && self.discard == 0 && self.window_starts_here()
        {
            self.window_drawn = true;
            self.bg_len = 0;
            self.fetcher = Fetcher::new(true, false);
            return;
        }
        let color = self.pop_bg();
        if self.lcd_x < 0 {
            self.lcd_x += 1;
            return;
        }
        if self.discard > 0 {
            self.discard -= 1;
            return;
        }
        let object = self.pop_obj();
        let bg_color = if self.lcdc & 0x01 != 0 { color } else { 0 };
        let shade =
            if object.color != 0 && self.lcdc & 0x02 != 0 && (!object.behind_bg || bg_color == 0) {
                let palette = if object.palette != 0 {
                    self.obp1
                } else {
                    self.obp0
                };
                (palette >> (object.color * 2)) & 3
            } else {
                (self.bgp >> (bg_color * 2)) & 3
            };
        self.frame[self.ly as usize * SCREEN_WIDTH + self.lcd_x as usize] = shade;
        self.lcd_x += 1;
    }

    fn window_starts_here(&self) -> bool {
        self.lcdc & 0x20 != 0
            && self.window_y_hit
            && self.wx <= 166
            && self.lcd_x + 7 >= self.wx as i16
    }

    fn pop_bg(&mut self) -> u8 {
        let color = self.bg_fifo[8 - self.bg_len as usize];
        self.bg_len -= 1;
        color
    }

    fn pop_obj(&mut self) -> ObjPixel {
        if self.obj_len == 0 {
            return ObjPixel::default();
        }
        let pixel = self.obj_fifo[0];
        self.obj_fifo.copy_within(1.., 0);
        self.obj_fifo[7] = ObjPixel::default();
        self.obj_len -= 1;
        pixel
    }

    fn advance_fetcher(&mut self) {
        let fetcher = self.fetcher;
        self.fetcher.since_push = fetcher.since_push.saturating_add(1);
        match fetcher.step {
            FetchStep::Push => {
                if self.bg_len == 0 {
                    for (bit, pixel) in (0..8).rev().zip(self.bg_fifo.iter_mut()) {
                        *pixel = (fetcher.low >> bit) & 1 | ((fetcher.high >> bit) & 1) << 1;
                    }
                    self.bg_len = 8;
                    self.fetcher.step = FetchStep::Tile;
                    self.fetcher.dot = 0;
                    self.fetcher.since_push = 0;
                    if fetcher.warmup {
                        self.fetcher.warmup = false;
                    } else {
                        self.fetcher.column = fetcher.column.wrapping_add(1);
                    }
                }
            }
            step => {
                if fetcher.dot == 0 {
                    self.fetcher.dot = 1;
                    return;
                }
                self.fetcher.dot = 0;
                match step {
                    FetchStep::Tile => {
                        self.fetcher.tile = self.vram[self.tile_map_address()];
                        self.fetcher.step = FetchStep::Low;
                    }
                    FetchStep::Low => {
                        self.fetcher.low = self.vram[self.tile_data_address()];
                        self.fetcher.step = FetchStep::High;
                    }
                    _ => {
                        self.fetcher.high = self.vram[self.tile_data_address() + 1];
                        self.fetcher.step = FetchStep::Push;
                    }
                }
            }
        }
    }

    fn tile_row(&self) -> u8 {
        if self.fetcher.window {
            self.window_line % 8
        } else {
            self.ly.wrapping_add(self.scy) % 8
        }
    }

    fn tile_map_address(&self) -> usize {
        let (map_bit, row, column) = if self.fetcher.window {
            (
                0x40,
                self.window_line as usize / 8,
                self.fetcher.column as usize,
            )
        } else {
            (
                0x08,
                self.ly.wrapping_add(self.scy) as usize / 8,
                (self.scx as usize / 8 + self.fetcher.column as usize) % 32,
            )
        };
        let map = if self.lcdc & map_bit != 0 {
            0x1C00
        } else {
            0x1800
        };
        map + row * 32 + column % 32
    }

    fn tile_data_address(&self) -> usize {
        let tile = self.fetcher.tile;
        let base = if self.lcdc & 0x10 != 0 {
            tile as usize * 16
        } else {
            (0x1000 + (tile as i8 as i32) * 16) as usize
        };
        base + self.tile_row() as usize * 2
    }

    fn fetch_sprite(&mut self, index: usize) {
        self.sprites[index].fetched = true;
        let sprite = self.sprites[index];
        let entry = sprite.oam_index as usize * 4;
        let flags = self.oam[entry + 3];
        let height = if self.lcdc & 0x04 != 0 { 16 } else { 8 };
        let mut row = (self.ly as i32 - (sprite.y as i32 - 16)) as u8 % height;
        if flags & 0x40 != 0 {
            row = height - 1 - row;
        }
        let mut tile = self.oam[entry + 2];
        if height == 16 {
            tile = tile & 0xFE | row / 8;
        }
        let address = tile as usize * 16 + (row % 8) as usize * 2;
        let (low, high) = (self.vram[address], self.vram[address + 1]);

        while self.obj_len < 8 {
            self.obj_fifo[self.obj_len as usize] = ObjPixel::default();
            self.obj_len += 1;
        }
        let skip = (self.lcd_x.max(0) + 8 - sprite.x as i16).max(0) as usize;
        for column in skip..8 {
            let bit = if flags & 0x20 != 0 {
                column
            } else {
                7 - column
            };
            let color = (low >> bit) & 1 | ((high >> bit) & 1) << 1;
            let slot = &mut self.obj_fifo[column - skip];
            if slot.color == 0 && color != 0 {
                *slot = ObjPixel {
                    color,
                    palette: (flags >> 4) & 1,
                    behind_bg: flags & 0x80 != 0,
                };
            }
        }
    }

    fn dots_until(&self, ly: u8, dot: u32) -> u32 {
        let now = self.ly as u32 * LINE_DOTS + self.dot;
        let then = ly as u32 * LINE_DOTS + dot;
        (then + FRAME_DOTS - now - 1) % FRAME_DOTS + 1
    }

    fn next_visible_line(&self) -> u8 {
        if self.ly + 1 < SCREEN_HEIGHT as u8 {
            self.ly + 1
        } else {
            0
        }
    }

    /// T-cycles until the PPU could next raise one of `enabled` interrupts.
    /// It may be early, never late.
    pub fn t_cycles_until_interrupt(&self, enabled: u8) -> usize {
        if !self.enabled() {
            return usize::MAX;
        }
        let stat = if enabled & STAT_INTERRUPT != 0 {
            self.stat
        } else {
            0
        };
        let mut dots = u32::MAX;
        if enabled & VBLANK_INTERRUPT != 0 || stat & (STAT_VBLANK | STAT_OAM) != 0 {
            dots = dots.min(self.dots_until(SCREEN_HEIGHT as u8, 0));
        }
        if stat & STAT_HBLANK != 0 {
            let earliest = OAM_SCAN_DOTS + MIN_TRANSFER_DOTS;
            let first_line_earliest = FIRST_LINE_TRANSFER_DOT + MIN_TRANSFER_DOTS;
            dots = dots.min(match self.mode {
                MODE_TRANSFER => earliest.saturating_sub(self.dot).max(1),
                MODE_OAM_SCAN => earliest - self.dot,
                MODE_HBLANK if self.first_line && self.dot < FIRST_LINE_TRANSFER_DOT => {
                    first_line_earliest - self.dot
                }
                _ => self.dots_until(self.next_visible_line(), earliest),
            });
        }
        if stat & STAT_OAM != 0 {
            dots = dots.min(self.dots_until(self.next_visible_line(), 0));
        }
        if stat & STAT_LYC != 0 && self.lyc < LINES {
            dots = dots.min(self.dots_until(self.lyc, 1));
        }
        if dots == u32::MAX {
            usize::MAX
        } else {
            dots as usize
        }
    }

    #[cfg(test)]
    pub fn describe(&self) -> String {
        format!(
            "ly {} dot {} mode {} visible {} lcdc {:02X} stat {:02X} lyc {} first_line {} stat_line {}",
            self.ly,
            self.dot,
            self.mode,
            self.visible_mode,
            self.lcdc,
            self.stat,
            self.lyc,
            self.first_line,
            self.stat_line
        )
    }

    pub fn read_vram(&self, addr: u16) -> u8 {
        self.vram[addr as usize & 0x1FFF]
    }

    pub fn write_vram(&mut self, addr: u16, value: u8) {
        self.vram[addr as usize & 0x1FFF] = value;
    }

    pub fn read_oam(&self, addr: u16) -> u8 {
        self.oam[addr as usize - 0xFE00]
    }

    pub fn write_oam(&mut self, addr: u16, value: u8) {
        self.oam[addr as usize - 0xFE00] = value;
    }

    pub fn write_oam_index(&mut self, index: usize, value: u8) {
        self.oam[index] = value;
    }

    pub fn read_register(&self, addr: u16) -> u8 {
        match addr {
            0xFF40 => self.lcdc,
            0xFF41 => {
                let mode = if self.enabled() {
                    self.visible_mode
                } else {
                    MODE_HBLANK
                };
                0x80 | self.stat | (self.lyc_match as u8) << 2 | mode
            }
            0xFF42 => self.scy,
            0xFF43 => self.scx,
            0xFF44 => self.ly,
            0xFF45 => self.lyc,
            0xFF47 => self.bgp,
            0xFF48 => self.obp0,
            0xFF49 => self.obp1,
            0xFF4A => self.wy,
            _ => self.wx,
        }
    }

    pub fn write_register(&mut self, addr: u16, value: u8) -> u8 {
        match addr {
            0xFF40 => {
                let was_enabled = self.enabled();
                self.lcdc = value;
                if was_enabled && !self.enabled() {
                    self.ly = 0;
                    self.dot = 0;
                    self.mode = MODE_HBLANK;
                    self.visible_mode = MODE_HBLANK;
                    self.window_line = 0;
                    self.window_drawn = false;
                    self.window_y_hit = false;
                    self.frame.fill(0);
                } else if !was_enabled && self.enabled() {
                    self.ly = 0;
                    self.dot = LCD_ON_DOT;
                    self.mode = MODE_HBLANK;
                    self.visible_mode = MODE_HBLANK;
                    self.first_line = true;
                    self.sprite_count = 0;
                }
            }
            0xFF41 => self.stat = value & 0x78,
            0xFF42 => self.scy = value,
            0xFF43 => self.scx = value,
            0xFF44 => {}
            0xFF45 => self.lyc = value,
            0xFF47 => self.bgp = value,
            0xFF48 => self.obp0 = value,
            0xFF49 => self.obp1 = value,
            0xFF4A => self.wy = value,
            _ => self.wx = value,
        }
        if self.enabled() {
            self.lyc_match = self.ly == self.lyc;
        }
        self.update_stat_line()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn transfer_dots(xs: &[u8], scx: u8) -> u32 {
        let mut ppu = Ppu::new();
        ppu.lcdc = 0x93;
        ppu.scx = scx;
        for (index, &x) in xs.iter().enumerate() {
            ppu.oam[index * 4] = 16;
            ppu.oam[index * 4 + 1] = x;
        }
        while ppu.mode != MODE_HBLANK {
            ppu.step();
        }
        ppu.dot - OAM_SCAN_DOTS
    }

    /// Pan Docs' rule for mode 3 extension: each object costs 6 dots, plus a
    /// wait for the background fetch of the first object in each tile.
    fn expected_penalty(xs: &[u8], scx: u8) -> u32 {
        let mut dots = 0;
        let mut waited_tiles = Vec::new();
        let mut sorted = xs.to_vec();
        sorted.sort();
        for x in sorted.into_iter().filter(|&x| x < 168) {
            let position = x as u32 + scx as u32 % 8;
            if !waited_tiles.contains(&(position / 8)) {
                waited_tiles.push(position / 8);
                dots += 5 - 5.min(position % 8);
            }
            dots += 6;
        }
        dots
    }

    #[test]
    fn sprite_penalties_match_hardware() {
        let mut cases: Vec<Vec<u8>> = vec![];
        for x in [
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 15, 16, 17, 32, 33, 160, 161, 162, 167, 168, 169,
        ] {
            cases.push(vec![x]);
            cases.push(vec![x; 10]);
        }
        for n in 1..=10 {
            cases.push(vec![0; n]);
        }
        cases.push(vec![0, 0, 0, 0, 0, 160, 160, 160, 160, 160]);
        cases.push(vec![64, 64, 64, 64, 64, 167, 167, 167, 167, 167]);
        cases.push(vec![0, 8]);
        cases.push(vec![1, 9]);
        let mut failures = Vec::new();
        for xs in &cases {
            let measured = transfer_dots(xs, 0) - MIN_TRANSFER_DOTS;
            let expected = expected_penalty(xs, 0);
            if measured != expected {
                failures.push(format!("{xs:?}: measured {measured}, expected {expected}"));
            }
        }
        assert!(failures.is_empty(), "{}", failures.join("\n"));
    }
}
