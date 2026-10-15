pub const SCREEN_WIDTH: usize = 160;
pub const SCREEN_HEIGHT: usize = 144;

const LINE_DOTS: u32 = 456;
const OAM_SCAN_DOTS: u32 = 80;
const TRANSFER_END_DOT: u32 = OAM_SCAN_DOTS + 172;
const LINES: u8 = 154;
const FRAME_DOTS: u32 = LINE_DOTS * LINES as u32;

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

/// The picture processing unit: video memory, OAM, LCD registers and a
/// scanline renderer that draws each line when the line's pixel transfer ends.
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
    dot: u32,
    window_line: u8,
    stat_line: bool,
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
            dot: 0,
            window_line: 0,
            stat_line: false,
            frame: Box::new([0; SCREEN_WIDTH * SCREEN_HEIGHT]),
        }
    }

    pub fn frame(&self) -> &[u8; SCREEN_WIDTH * SCREEN_HEIGHT] {
        &self.frame
    }

    fn enabled(&self) -> bool {
        self.lcdc & 0x80 != 0
    }

    fn mode(&self) -> u8 {
        if !self.enabled() {
            MODE_HBLANK
        } else if self.ly >= SCREEN_HEIGHT as u8 {
            MODE_VBLANK
        } else if self.dot < OAM_SCAN_DOTS {
            MODE_OAM_SCAN
        } else if self.dot < TRANSFER_END_DOT {
            MODE_TRANSFER
        } else {
            MODE_HBLANK
        }
    }

    fn update_stat_line(&mut self) -> u8 {
        let mode = self.mode();
        let line = self.enabled()
            && (self.stat & STAT_LYC != 0 && self.ly == self.lyc
                || self.stat & STAT_HBLANK != 0 && mode == MODE_HBLANK
                || self.stat & STAT_VBLANK != 0 && mode == MODE_VBLANK
                || self.stat & STAT_OAM != 0 && mode == MODE_OAM_SCAN);
        let rising = line && !self.stat_line;
        self.stat_line = line;
        if rising { STAT_INTERRUPT } else { 0 }
    }

    pub fn tick(&mut self, t_cycles: usize) -> u8 {
        if !self.enabled() {
            return 0;
        }
        let mut interrupts = 0;
        let mut remaining = t_cycles as u32;
        while remaining > 0 {
            let boundary = if self.ly >= SCREEN_HEIGHT as u8 || self.dot >= TRANSFER_END_DOT {
                LINE_DOTS
            } else if self.dot >= OAM_SCAN_DOTS {
                TRANSFER_END_DOT
            } else {
                OAM_SCAN_DOTS
            };
            let step = remaining.min(boundary - self.dot);
            self.dot += step;
            remaining -= step;
            if self.dot == TRANSFER_END_DOT && self.ly < SCREEN_HEIGHT as u8 {
                self.render_line();
            }
            if self.dot == LINE_DOTS {
                self.dot = 0;
                self.ly = (self.ly + 1) % LINES;
                if self.ly == SCREEN_HEIGHT as u8 {
                    interrupts |= VBLANK_INTERRUPT;
                }
                if self.ly == 0 {
                    self.window_line = 0;
                }
            }
            interrupts |= self.update_stat_line();
        }
        interrupts
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
        if enabled & VBLANK_INTERRUPT != 0 || stat & STAT_VBLANK != 0 {
            dots = dots.min(self.dots_until(SCREEN_HEIGHT as u8, 0));
        }
        if stat & STAT_HBLANK != 0 {
            let this_line = self.ly < SCREEN_HEIGHT as u8 && self.dot < TRANSFER_END_DOT;
            let ly = if this_line {
                self.ly
            } else {
                self.next_visible_line()
            };
            dots = dots.min(self.dots_until(ly, TRANSFER_END_DOT));
        }
        if stat & STAT_OAM != 0 {
            dots = dots.min(self.dots_until(self.next_visible_line(), 0));
        }
        if stat & STAT_LYC != 0 && self.lyc < LINES {
            dots = dots.min(self.dots_until(self.lyc, 0));
        }
        if dots == u32::MAX {
            usize::MAX
        } else {
            dots as usize
        }
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
                0x80 | self.stat
                    | ((self.enabled() && self.ly == self.lyc) as u8) << 2
                    | self.mode()
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
                    self.window_line = 0;
                    self.frame.fill(0);
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
        self.update_stat_line()
    }

    fn tile_row(&self, tile: u8, row: u8, signed_addressing: bool) -> (u8, u8) {
        let base = if signed_addressing {
            (0x1000 + (tile as i8 as i32) * 16) as usize
        } else {
            tile as usize * 16
        };
        let offset = base + row as usize * 2;
        (self.vram[offset], self.vram[offset + 1])
    }

    fn render_line(&mut self) {
        let ly = self.ly;
        let mut bg_colors = [0u8; SCREEN_WIDTH];
        let signed_addressing = self.lcdc & 0x10 == 0;

        if self.lcdc & 0x01 != 0 {
            let map = if self.lcdc & 0x08 != 0 {
                0x1C00
            } else {
                0x1800
            };
            let y = ly.wrapping_add(self.scy);
            for (x, color) in bg_colors.iter_mut().enumerate() {
                let px = (x as u8).wrapping_add(self.scx);
                let tile = self.vram[map + (y as usize / 8) * 32 + px as usize / 8];
                let (lo, hi) = self.tile_row(tile, y % 8, signed_addressing);
                let bit = 7 - px % 8;
                *color = (lo >> bit) & 1 | ((hi >> bit) & 1) << 1;
            }

            let window_x = self.wx as i32 - 7;
            if self.lcdc & 0x20 != 0 && ly >= self.wy && window_x < SCREEN_WIDTH as i32 {
                let map = if self.lcdc & 0x40 != 0 {
                    0x1C00
                } else {
                    0x1800
                };
                let y = self.window_line;
                for x in window_x.max(0) as usize..SCREEN_WIDTH {
                    let wx = (x as i32 - window_x) as usize;
                    let tile = self.vram[map + (y as usize / 8) * 32 + wx / 8];
                    let (lo, hi) = self.tile_row(tile, y % 8, signed_addressing);
                    let bit = 7 - (wx % 8) as u8;
                    bg_colors[x] = (lo >> bit) & 1 | ((hi >> bit) & 1) << 1;
                }
                self.window_line += 1;
            }
        }

        let mut line = bg_colors.map(|color| (self.bgp >> (color * 2)) & 3);
        if self.lcdc & 0x02 != 0 {
            self.draw_sprites(&mut line, &bg_colors);
        }
        self.frame[ly as usize * SCREEN_WIDTH..][..SCREEN_WIDTH].copy_from_slice(&line);
    }

    fn draw_sprites(&self, line: &mut [u8; SCREEN_WIDTH], bg_colors: &[u8; SCREEN_WIDTH]) {
        let ly = self.ly;
        let height = if self.lcdc & 0x04 != 0 { 16 } else { 8 };
        let mut sprites: Vec<(u8, usize)> = (0..40)
            .filter(|&index| {
                let top = self.oam[index * 4] as i32 - 16;
                (top..top + height).contains(&(ly as i32))
            })
            .take(10)
            .map(|index| (self.oam[index * 4 + 1], index))
            .collect();
        sprites.sort();

        for x in 0..SCREEN_WIDTH {
            for &(sprite_x, index) in &sprites {
                let left = sprite_x as i32 - 8;
                if !(left..left + 8).contains(&(x as i32)) {
                    continue;
                }
                let [y, _, mut tile, flags] = self.oam[index * 4..index * 4 + 4] else {
                    unreachable!()
                };
                let mut row = (ly as i32 - (y as i32 - 16)) as u8;
                if flags & 0x40 != 0 {
                    row = height as u8 - 1 - row;
                }
                if height == 16 {
                    tile = tile & 0xFE | row / 8;
                }
                let (lo, hi) = self.tile_row(tile, row % 8, false);
                let column = (x as i32 - left) as u8;
                let bit = if flags & 0x20 != 0 {
                    column
                } else {
                    7 - column
                };
                let color = (lo >> bit) & 1 | ((hi >> bit) & 1) << 1;
                if color == 0 {
                    continue;
                }
                if flags & 0x80 == 0 || bg_colors[x] == 0 {
                    let palette = if flags & 0x10 != 0 {
                        self.obp1
                    } else {
                        self.obp0
                    };
                    line[x] = (palette >> (color * 2)) & 3;
                }
                break;
            }
        }
    }
}
