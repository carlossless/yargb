pub const SCREEN_WIDTH: usize = 160;
pub const SCREEN_HEIGHT: usize = 144;

const LINE_DOTS: u32 = 456;
const OAM_SCAN_DOTS: u32 = 80;
const MIN_TRANSFER_DOTS: u32 = 172;
const LINES: u8 = 154;
const FRAME_DOTS: u32 = LINE_DOTS * LINES as u32;
const MAX_LINE_SPRITES: usize = 10;
/// Dots before line 144 at which a CGB raises the mode 2 STAT interrupt
/// that the DMG raises together with VBlank.
const CGB_VBLANK_OAM_LEAD: u32 = 4;
/// Dots before STAT and the VRAM/OAM locks reflect a switch to mode 3,
/// counting the dot of the switch. Interrupts see the switch immediately.
const VISIBLE_MODE_LAG: u8 = 2;
/// Turning the LCD on drops the PPU into line 0 this many dots in.
const LCD_ON_DOT: u32 = 3;
/// Line 0 after turning the LCD on has no OAM scan and starts its transfer
/// 2 dots later than other lines.
const FIRST_LINE_TRANSFER_DOT: u32 = OAM_SCAN_DOTS + 2;
/// Dots between STAT reporting mode 3 and the pixel pipeline starting.
const PIPELINE_START_DELAY: u8 = 5;
/// The OAM scan moves to the next 8-byte row every 4 dots, this many dots
/// before the dot count reaches a multiple of 4.
const OAM_BUG_ROW_LAG: u32 = 5;

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

pub const LCDC_ENABLE: u8 = 0x80;
const LCDC_WIN_MAP: u8 = 0x40;
const LCDC_WIN_ENABLE: u8 = 0x20;
const LCDC_TILE_SEL: u8 = 0x10;
const LCDC_BG_MAP: u8 = 0x08;
const LCDC_OBJ_SIZE: u8 = 0x04;
pub const LCDC_OBJ_ENABLE: u8 = 0x02;
pub const LCDC_BG_ENABLE: u8 = 0x01;

/// Background fetcher steps. A VRAM read takes two dots: the address is set
/// up in the first and the data arrives in the second.
const FETCH_TILE_T1: u8 = 0;
const FETCH_TILE_T2: u8 = 1;
const FETCH_LOW_T1: u8 = 2;
const FETCH_LOW_T2: u8 = 3;
const FETCH_HIGH_T1: u8 = 4;
const FETCH_HIGH_T2: u8 = 5;
const FETCH_PUSH: u8 = 6;

/// Positions in the line are kept as wrapping bytes: the 16 before the
/// first visible pixel are 240 to 255.
const POSITION_START: u8 = 0u8.wrapping_sub(16);

#[derive(Clone, Copy, Default, PartialEq)]
struct FifoItem {
    pixel: u8,
    palette: u8,
    priority: u8,
    bg_priority: bool,
}

#[derive(Clone, Copy, Default, PartialEq)]
struct Fifo {
    items: [FifoItem; 8],
    read_end: u8,
    size: u8,
}

impl Fifo {
    fn clear(&mut self) {
        self.read_end = 0;
        self.size = 0;
    }

    fn pop(&mut self) -> FifoItem {
        let item = self.items[self.read_end as usize];
        self.read_end = (self.read_end + 1) & 7;
        self.size -= 1;
        item
    }

    fn push_bg_row(
        &mut self,
        mut lower: u8,
        mut upper: u8,
        palette: u8,
        bg_priority: bool,
        flip_x: bool,
    ) {
        self.size = 8;
        for item in &mut self.items {
            let pixel = if flip_x {
                let pixel = (lower & 1) | (upper & 1) << 1;
                lower >>= 1;
                upper >>= 1;
                pixel
            } else {
                let pixel = (lower >> 7) | (upper >> 7) << 1;
                lower <<= 1;
                upper <<= 1;
                pixel
            };
            *item = FifoItem {
                pixel,
                palette,
                priority: 0,
                bg_priority,
            };
        }
    }

    /// Objects already in the FIFO keep their pixels unless the new object
    /// wins on priority.
    fn overlay_object_row(
        &mut self,
        mut lower: u8,
        mut upper: u8,
        palette: u8,
        bg_priority: bool,
        priority: u8,
        flip_x: bool,
    ) {
        while self.size < 8 {
            self.items[((self.read_end + self.size) & 7) as usize] = FifoItem::default();
            self.size += 1;
        }
        let flip_xor = if flip_x { 0 } else { 7 };
        for i in (0..8u8).rev() {
            let pixel = (lower >> 7) | (upper >> 7) << 1;
            let target = &mut self.items[((self.read_end + (i ^ flip_xor)) & 7) as usize];
            if pixel != 0 && (target.pixel == 0 || target.priority > priority) {
                *target = FifoItem {
                    pixel,
                    palette,
                    priority,
                    bg_priority,
                };
            }
            lower <<= 1;
            upper <<= 1;
        }
    }
}

/// Where the mode 3 pipeline resumes on its next dot.
#[derive(Clone, Copy, PartialEq)]
enum Pipeline {
    Delay(u8),
    Window,
    AfterWindow,
    Objects,
    ObjectWait,
    AfterObjectWait,
    ObjectAttributes,
    ObjectLow,
    ObjectHigh,
    ObjectPush,
    Pixel,
}

fn palette_color(palettes: &[u8; 64], palette: u8, color: u8) -> u16 {
    let index = (palette as usize * 4 + color as usize) * 2;
    u16::from_le_bytes([palettes[index], palettes[index + 1]]) & 0x7FFF
}

/// The picture processing unit, emulated dot by dot. Pixels come out of a
/// background FIFO fed by a tile fetcher, mixed with an object FIFO, so the
/// length of mode 3 and the effect of mid-line register writes follow from the
/// fetches the hardware makes.
#[derive(PartialEq)]
pub struct Ppu {
    cgb: bool,
    compat: bool,
    vram: [u8; 0x4000],
    vram_bank: usize,
    bg_palettes: [u8; 64],
    obj_palettes: [u8; 64],
    bg_palette_index: u8,
    obj_palette_index: u8,
    x_priority: bool,
    hblank_started: bool,
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
    stat_mode: u8,
    stat_lag: u8,
    visible_lag: u8,
    dot: u32,
    stat_line: bool,
    lyc_match: bool,
    first_line: bool,
    frame: Box<Frame>,
    pipeline: Pipeline,
    pipeline_sleep: u8,
    bg_fifo: Fifo,
    obj_fifo: Fifo,
    position_in_line: u8,
    lcd_x: u8,
    fetcher_state: u8,
    fetcher_y: u8,
    current_tile: u8,
    current_tile_attributes: u8,
    current_tile_data: [u8; 2],
    last_tile_index_address: u16,
    last_tile_data_address: u16,
    wx_triggered: bool,
    wy_triggered: bool,
    window_y: u8,
    window_tile_x: u8,
    window_is_being_fetched: bool,
    insert_bg_pixel: bool,
    line_has_fractional_scrolling: bool,
    disable_window_pixel_insertion_glitch: bool,
    /// Set for one dot after a DMG write to WX.
    pub wx_just_changed: bool,
    cgb_wx_glitch: bool,
    wy_check_pending: bool,
    visible_objs: [u8; MAX_LINE_SPRITES],
    objects_x: [u8; MAX_LINE_SPRITES],
    objects_y: [u8; MAX_LINE_SPRITES],
    n_visible_objs: usize,
    object_tile: u8,
    object_flags: u8,
    object_tile_data: [u8; 2],
    object_line_address: u16,
    during_object_fetch: bool,
    object_fetch_aborted: bool,
}

/// One pixel per screen position: a DMG shade from 0 (lightest) to 3, or a
/// CGB colour in RGB555.
pub type Frame = [u16; SCREEN_WIDTH * SCREEN_HEIGHT];

impl Ppu {
    /// `compat` is a CGB running a DMG cartridge: DMG rendering, coloured
    /// through palette RAM.
    pub fn new(cgb: bool, compat: bool) -> Ppu {
        let mut bg_palettes = [0xFF; 64];
        let mut obj_palettes = [0xFF; 64];
        if compat {
            let greys = [0x7FFFu16, 0x56B5, 0x294A, 0x0000];
            let bytes: Vec<u8> = greys.iter().flat_map(|color| color.to_le_bytes()).collect();
            bg_palettes[..8].copy_from_slice(&bytes);
            obj_palettes[..8].copy_from_slice(&bytes);
            obj_palettes[8..16].copy_from_slice(&bytes);
        }
        Ppu {
            cgb,
            compat,
            vram: [0; 0x4000],
            vram_bank: 0,
            bg_palettes,
            obj_palettes,
            bg_palette_index: 0,
            obj_palette_index: 0,
            x_priority: !cgb,
            hblank_started: false,
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
            stat_mode: MODE_OAM_SCAN,
            stat_lag: 0,
            visible_lag: 0,
            dot: 0,
            stat_line: false,
            lyc_match: true,
            first_line: false,
            frame: Box::new([0; SCREEN_WIDTH * SCREEN_HEIGHT]),
            pipeline: Pipeline::Delay(0),
            pipeline_sleep: 0,
            bg_fifo: Fifo::default(),
            obj_fifo: Fifo::default(),
            position_in_line: POSITION_START,
            lcd_x: 0,
            fetcher_state: FETCH_TILE_T1,
            fetcher_y: 0,
            current_tile: 0,
            current_tile_attributes: 0,
            current_tile_data: [0; 2],
            last_tile_index_address: 0,
            last_tile_data_address: 0,
            wx_triggered: false,
            wy_triggered: false,
            window_y: 0xFF,
            window_tile_x: 0,
            window_is_being_fetched: false,
            insert_bg_pixel: false,
            line_has_fractional_scrolling: false,
            disable_window_pixel_insertion_glitch: false,
            wx_just_changed: false,
            cgb_wx_glitch: false,
            wy_check_pending: false,
            visible_objs: [0; MAX_LINE_SPRITES],
            objects_x: [0; MAX_LINE_SPRITES],
            objects_y: [0; MAX_LINE_SPRITES],
            n_visible_objs: 0,
            object_tile: 0,
            object_flags: 0,
            object_tile_data: [0; 2],
            object_line_address: 0,
            during_object_fetch: false,
            object_fetch_aborted: false,
        }
    }

    /// Leaves VRAM as the DMG boot ROM does: the cartridge's logo scaled up
    /// into tiles 1-24, the ® mark in tile 25, and both placed on the map.
    pub fn load_dmg_boot_screen(&mut self, logo: &[u8; 48]) {
        let mut address = 0x10;
        for &byte in logo {
            for nibble in [byte >> 4, byte & 0x0F] {
                let doubled = (0..4).rev().fold(0u8, |row, bit| {
                    let set = (nibble >> bit) & 1;
                    row << 2 | set << 1 | set
                });
                self.vram[address] = doubled;
                self.vram[address + 2] = doubled;
                address += 4;
            }
        }
        const REGISTERED: [u8; 8] = [0x3C, 0x42, 0xB9, 0xA5, 0xB9, 0xA5, 0x42, 0x3C];
        for (row, &byte) in REGISTERED.iter().enumerate() {
            self.vram[0x190 + row * 2] = byte;
        }
        self.vram[0x1910] = 0x19;
        for column in 0..12 {
            self.vram[0x1904 + column] = column as u8 + 1;
            self.vram[0x1924 + column] = column as u8 + 13;
        }
    }

    pub fn frame(&self) -> &Frame {
        &self.frame
    }

    /// Whether frames hold RGB555 colours rather than DMG shades.
    pub fn renders_color(&self) -> bool {
        self.cgb || self.compat
    }

    /// Whether mode 0 began on a visible line since the last call, which is
    /// when an HBlank DMA copies its next block.
    pub fn take_hblank_started(&mut self) -> bool {
        std::mem::take(&mut self.hblank_started)
    }

    fn blank_color(&self) -> u16 {
        if self.renders_color() { 0x7FFF } else { 0 }
    }

    fn enabled(&self) -> bool {
        self.lcdc & LCDC_ENABLE != 0
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

    /// STAT and the memory locks see the start of mode 3 a dot late, and its
    /// end straight away.
    fn update_visible_mode(&mut self) {
        if self.visible_mode == self.mode {
            self.visible_lag = 0;
        } else if self.mode == MODE_VBLANK || self.mode == MODE_HBLANK {
            self.visible_mode = self.mode;
        } else {
            if self.visible_lag == 0 {
                self.visible_lag = VISIBLE_MODE_LAG;
            }
            self.visible_lag -= 1;
            if self.visible_lag == 0 {
                self.visible_mode = self.mode;
            }
        }
        self.update_stat_mode();
    }

    /// The STAT interrupt sees HBlank a dot after the memory locks lift, and
    /// on the DMG sees VBlank two dots after it begins.
    fn update_stat_mode(&mut self) {
        if self.stat_mode == self.visible_mode {
            self.stat_lag = 0;
            return;
        }
        let lag = match self.visible_mode {
            MODE_HBLANK => 1,
            MODE_VBLANK if !self.renders_color() => 2,
            _ => 0,
        };
        self.stat_lag += 1;
        if self.stat_lag > lag {
            self.stat_mode = self.visible_mode;
            self.stat_lag = 0;
        }
    }

    /// The mode 2 STAT source also fires once around the start of VBlank, though
    /// no OAM scan follows.
    fn oam_interrupt_at_vblank(&self) -> bool {
        if self.renders_color() {
            self.ly == SCREEN_HEIGHT as u8 - 1 && self.dot == LINE_DOTS - CGB_VBLANK_OAM_LEAD
        } else {
            self.ly == SCREEN_HEIGHT as u8 && self.dot == self.cpu_visible_dot(self.ly)
        }
    }

    /// The dot at which the mode a line starts with reaches the interrupt
    /// logic: on the DMG, line 0 and VBlank arrive late.
    fn cpu_visible_dot(&self, ly: u8) -> u32 {
        match ly {
            _ if self.renders_color() => 0,
            0 => 1,
            ly if ly == SCREEN_HEIGHT as u8 => 2,
            _ => 0,
        }
    }

    fn dmg_mode_change_pending(&self) -> bool {
        self.dot < self.cpu_visible_dot(self.ly)
    }

    fn update_stat_line(&mut self) -> u8 {
        let mode = match self.mode {
            _ if !self.dmg_mode_change_pending() => self.mode,
            MODE_VBLANK => MODE_HBLANK,
            _ => MODE_VBLANK,
        };
        let line = self.stat & STAT_LYC != 0 && self.lyc_match
            || self.enabled()
                && (self.stat & STAT_HBLANK != 0
                    && mode == MODE_HBLANK
                    && (self.stat_mode == MODE_HBLANK || mode != self.mode)
                    || self.stat & STAT_VBLANK != 0 && mode == MODE_VBLANK
                    || self.stat & STAT_OAM != 0
                        && (mode == MODE_OAM_SCAN || self.oam_interrupt_at_vblank()));
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
            if self.settled_in_transfer() {
                while remaining > 0 {
                    remaining -= 1;
                    if self.pipeline_dot() {
                        self.end_transfer();
                        self.dot += 1;
                        self.update_visible_mode();
                        interrupts |= self.update_stat_line();
                        break;
                    }
                    self.dot += 1;
                }
                continue;
            }
            let quiet = self.quiet_dots().min(remaining);
            if quiet > 0 {
                self.skip_quiet_dots(quiet);
                remaining -= quiet;
            } else {
                interrupts |= self.step();
                remaining -= 1;
            }
        }
        interrupts
    }

    /// Whether the coming dots of mode 3 can run the pixel pipeline alone:
    /// past the OAM scan, with STAT settled and no pending register effects.
    fn settled_in_transfer(&self) -> bool {
        self.mode == MODE_TRANSFER
            && self.visible_mode == MODE_TRANSFER
            && self.stat_mode == MODE_TRANSFER
            && self.dot > OAM_SCAN_DOTS
            && !self.wx_just_changed
            && !self.wy_check_pending
    }

    /// Dots from now in which stepping would change nothing but the dot
    /// counter and the OAM scan, so they can be skipped in one go. Each run
    /// stops a dot short of the next event for `step` to handle.
    fn quiet_dots(&self) -> u32 {
        if self.visible_mode != self.mode
            || self.stat_mode != self.visible_mode
            || self.dot == 0
            || self.wx_just_changed
            || self.wy_check_pending
        {
            return 0;
        }
        let end = match self.mode {
            MODE_OAM_SCAN => OAM_SCAN_DOTS - 1,
            MODE_HBLANK if self.first_line && self.dot < FIRST_LINE_TRANSFER_DOT => {
                FIRST_LINE_TRANSFER_DOT - 1
            }
            MODE_HBLANK | MODE_VBLANK
                if self.renders_color() && self.ly == SCREEN_HEIGHT as u8 - 1 =>
            {
                if self.dot < LINE_DOTS - CGB_VBLANK_OAM_LEAD {
                    LINE_DOTS - CGB_VBLANK_OAM_LEAD - 1
                } else {
                    LINE_DOTS - 1
                }
            }
            MODE_HBLANK | MODE_VBLANK => LINE_DOTS - 1,
            _ => return 0,
        };
        end.saturating_sub(self.dot)
    }

    fn skip_quiet_dots(&mut self, dots: u32) {
        if self.mode == MODE_OAM_SCAN {
            for dot in self.dot..self.dot + dots {
                self.scan_oam_at(dot);
            }
        }
        self.dot += dots;
    }

    /// The OAM scan reads an entry every 2 dots: on the CGB as each pair of
    /// dots starts, on the DMG as it ends.
    fn scan_oam_at(&mut self, dot: u32) {
        if dot % 2 != 0 {
            return;
        }
        let index = if self.renders_color() {
            dot / 2
        } else if dot >= 2 {
            dot / 2 - 1
        } else {
            return;
        };
        if index < 40 {
            self.add_object(index as usize);
        }
    }

    fn step(&mut self) -> u8 {
        let interrupts = self.step_inner();
        self.wx_just_changed = false;
        interrupts
    }

    fn step_inner(&mut self) -> u8 {
        let mut interrupts = 0;
        if self.wy_check_pending {
            self.wy_check_pending = false;
            self.wy_check(self.ly);
        }
        match self.mode {
            MODE_OAM_SCAN => {
                self.scan_oam_at(self.dot);
                if self.dot + 1 == OAM_SCAN_DOTS {
                    self.start_transfer();
                }
            }
            MODE_TRANSFER => {
                if !self.first_line {
                    self.scan_oam_at(self.dot);
                }
                if self.pipeline_dot() {
                    self.end_transfer();
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
        } else if self.ly == SCREEN_HEIGHT as u8
            && !self.renders_color()
            && self.dot == self.cpu_visible_dot(self.ly)
        {
            interrupts |= VBLANK_INTERRUPT;
        }
        self.update_visible_mode();
        self.lyc_match = self.dot != 0 && self.ly == self.lyc;
        interrupts | self.update_stat_line()
    }

    fn next_line(&mut self) -> u8 {
        let previous = self.ly;
        self.dot = 0;
        self.first_line = false;
        self.ly = (self.ly + 1) % LINES;
        if self.ly == 0 {
            self.wy_triggered = false;
        }
        if self.ly < SCREEN_HEIGHT as u8 {
            self.mode = MODE_OAM_SCAN;
            self.n_visible_objs = 0;
            self.wy_check(if self.ly == 0 { 0 } else { previous });
            self.wy_check(self.ly);
            0
        } else if self.ly == SCREEN_HEIGHT as u8 {
            self.mode = MODE_VBLANK;
            if self.renders_color() {
                VBLANK_INTERRUPT
            } else {
                0
            }
        } else {
            0
        }
    }

    fn wy_check(&mut self, ly: u8) {
        if self.enabled() && self.lcdc & LCDC_WIN_ENABLE != 0 && self.wy == ly {
            self.wy_triggered = true;
        }
    }

    fn add_object(&mut self, index: usize) {
        if self.n_visible_objs == MAX_LINE_SPRITES {
            return;
        }
        let y = self.oam[index * 4];
        let x = self.oam[index * 4 + 1];
        let height = if self.lcdc & LCDC_OBJ_SIZE != 0 {
            16
        } else {
            8
        };
        let top = y as i32 - 16;
        if top <= self.ly as i32 && top + height > self.ly as i32 {
            let count = self.n_visible_objs;
            let slot = (0..count)
                .find(|&j| self.objects_x[j] <= x)
                .unwrap_or(count);
            self.visible_objs.copy_within(slot..count, slot + 1);
            self.objects_x.copy_within(slot..count, slot + 1);
            self.objects_y.copy_within(slot..count, slot + 1);
            self.visible_objs[slot] = index as u8;
            self.objects_x[slot] = x;
            self.objects_y[slot] = y;
            self.n_visible_objs += 1;
        }
    }

    fn start_transfer(&mut self) {
        self.mode = MODE_TRANSFER;
        self.pipeline = Pipeline::Delay(PIPELINE_START_DELAY);
        self.pipeline_sleep = 0;
    }

    fn end_transfer(&mut self) {
        self.position_in_line = POSITION_START;
        self.line_has_fractional_scrolling = false;
        if self.fetcher_state == FETCH_HIGH_T1 || self.fetcher_state == FETCH_HIGH_T2 {
            self.current_tile_data[1] = self.current_tile_data[0];
        }
        if self.ly < SCREEN_HEIGHT as u8 {
            let row = self.ly as usize * SCREEN_WIDTH;
            while (self.lcd_x as usize) < SCREEN_WIDTH {
                let x = self.lcd_x as usize;
                self.frame[row + x] = if x == 0 {
                    self.blank_color()
                } else {
                    self.frame[row + x - 1]
                };
                self.lcd_x += 1;
            }
        }
        if self.ly == SCREEN_HEIGHT as u8 - 1 {
            self.window_y = 0xFF;
        }
        if !self.renders_color()
            && self.wy_triggered
            && self.lcdc & LCDC_WIN_ENABLE != 0
            && self.wx == 166
        {
            self.wx_triggered = true;
            self.window_tile_x = 1;
            self.window_y = self.window_y.wrapping_add(1);
        } else {
            self.wx_triggered = false;
        }
        self.mode = MODE_HBLANK;
        self.hblank_started = true;
    }

    fn start_pipeline(&mut self) {
        self.disable_window_pixel_insertion_glitch = false;
        self.bg_fifo.clear();
        self.obj_fifo.clear();
        self.bg_fifo.push_bg_row(0, 0, 0, false, false);
        self.lcd_x = 0;
        self.fetcher_state = FETCH_TILE_T1;
        if self.first_line {
            self.wx_triggered = false;
        }
    }

    fn x_for_object_match(&self) -> u8 {
        let x = self.position_in_line.wrapping_add(8);
        if x > POSITION_START { 0 } else { x }
    }

    fn objects_enabled(&self) -> bool {
        self.lcdc & LCDC_OBJ_ENABLE != 0 || self.renders_color()
    }

    fn object_at_match(&self) -> bool {
        self.n_visible_objs != 0
            && self.objects_enabled()
            && self.objects_x[self.n_visible_objs - 1] == self.x_for_object_match()
    }

    /// Runs one dot of mode 3 and returns whether the line is finished.
    fn pipeline_dot(&mut self) -> bool {
        if self.pipeline_sleep > 0 {
            self.pipeline_sleep -= 1;
            return false;
        }
        if self.pipeline == Pipeline::Window
            && !self.window_event_possible()
            && (self.n_visible_objs == 0
                || self.objects_x[self.n_visible_objs - 1] > self.x_for_object_match())
        {
            self.object_fetch_aborted = false;
            self.render_pixel();
            self.advance_fetcher();
            return self.position_in_line == SCREEN_WIDTH as u8;
        }
        loop {
            match self.pipeline {
                Pipeline::Delay(0) => {
                    self.start_pipeline();
                    self.pipeline = Pipeline::Window;
                }
                Pipeline::Delay(dots) => {
                    self.pipeline = Pipeline::Delay(dots - 1);
                    return false;
                }
                Pipeline::Window => {
                    self.pipeline = Pipeline::AfterWindow;
                    if self.activate_window() {
                        return false;
                    }
                }
                Pipeline::AfterWindow => {
                    if self.wx == self.position_in_line.wrapping_add(7)
                        && (!self.renders_color() || self.wx == 0)
                        && self.wx_triggered
                        && !self.window_is_being_fetched
                        && self.fetcher_state == FETCH_TILE_T1
                        && self.bg_fifo.size == 8
                    {
                        self.insert_bg_pixel = true;
                    }
                    while self.n_visible_objs != 0
                        && self.objects_x[self.n_visible_objs - 1] < self.x_for_object_match()
                    {
                        self.n_visible_objs -= 1;
                    }
                    self.during_object_fetch = true;
                    self.pipeline = Pipeline::Objects;
                }
                Pipeline::Objects => {
                    self.pipeline = if self.object_at_match() {
                        Pipeline::ObjectWait
                    } else {
                        Pipeline::Pixel
                    };
                }
                Pipeline::ObjectWait => {
                    let waiting = self.fetcher_state < FETCH_HIGH_T2 || self.bg_fifo.size == 0;
                    self.advance_fetcher();
                    self.pipeline = if waiting {
                        Pipeline::AfterObjectWait
                    } else {
                        Pipeline::ObjectAttributes
                    };
                    return false;
                }
                Pipeline::AfterObjectWait => {
                    self.pipeline = if self.object_fetch_aborted {
                        Pipeline::Pixel
                    } else {
                        Pipeline::ObjectWait
                    };
                }
                Pipeline::ObjectAttributes => {
                    if self.object_fetch_aborted {
                        self.pipeline = Pipeline::Pixel;
                        continue;
                    }
                    self.advance_fetcher();
                    let entry = self.visible_objs[self.n_visible_objs - 1] as usize * 4;
                    self.object_tile = self.oam[entry + 2];
                    self.object_flags = self.oam[entry + 3];
                    self.pipeline = Pipeline::ObjectLow;
                    self.pipeline_sleep = 1;
                    return false;
                }
                Pipeline::ObjectLow => {
                    if self.object_fetch_aborted {
                        self.pipeline = Pipeline::Pixel;
                        continue;
                    }
                    self.object_line_address = self.object_line_address();
                    self.object_tile_data[0] = self.vram[self.object_line_address as usize];
                    self.pipeline = Pipeline::ObjectHigh;
                    self.pipeline_sleep = 1;
                    return false;
                }
                Pipeline::ObjectHigh => {
                    if self.object_fetch_aborted {
                        self.pipeline = Pipeline::Pixel;
                        continue;
                    }
                    self.during_object_fetch = false;
                    self.object_line_address = self.object_line_address();
                    self.object_tile_data[1] = self.vram[self.object_line_address as usize + 1];
                    self.pipeline = Pipeline::ObjectPush;
                    return false;
                }
                Pipeline::ObjectPush => {
                    let flags = self.object_flags;
                    let palette = if self.cgb {
                        flags & 0x07
                    } else {
                        (flags >> 4) & 1
                    };
                    let priority = if self.x_priority {
                        0
                    } else {
                        self.visible_objs[self.n_visible_objs - 1]
                    };
                    let [lower, upper] = self.object_tile_data;
                    self.obj_fifo.overlay_object_row(
                        lower,
                        upper,
                        palette,
                        flags & 0x80 != 0,
                        priority,
                        flags & 0x20 != 0,
                    );
                    self.n_visible_objs -= 1;
                    self.pipeline = Pipeline::Objects;
                }
                Pipeline::Pixel => {
                    self.object_fetch_aborted = false;
                    self.during_object_fetch = false;
                    self.render_pixel();
                    self.advance_fetcher();
                    if self.position_in_line == SCREEN_WIDTH as u8 {
                        return true;
                    }
                    self.pipeline = Pipeline::Window;
                    return false;
                }
            }
        }
    }

    /// Whether the window could start, or have a pixel inserted, at this
    /// position. When it can't, a dot of mode 3 needs no window handling.
    fn window_event_possible(&self) -> bool {
        let next = self.position_in_line.wrapping_add(7);
        if self.wx_triggered {
            return self.wx == next;
        }
        self.wy_triggered
            && self.lcdc & LCDC_WIN_ENABLE != 0
            && (self.wx == 0 || self.wx == next || self.wx == next.wrapping_sub(1))
    }

    /// Starts fetching the window if it begins here, and returns whether that
    /// takes an extra dot.
    fn activate_window(&mut self) -> bool {
        if self.wx_triggered || !self.wy_triggered || self.lcdc & LCDC_WIN_ENABLE == 0 {
            return false;
        }
        let position = self.position_in_line;
        let cgb = self.renders_color();
        let mut activate = false;
        if self.wx == 0 {
            activate = position == 0u8.wrapping_sub(7)
                || position == POSITION_START && self.scx & 7 != 0
                || (0u8.wrapping_sub(15)..=0u8.wrapping_sub(8)).contains(&position);
        } else if (self.wx as u16) < 166 + cgb as u16 {
            activate = self.wx == position.wrapping_add(7)
                || !cgb && self.wx == position.wrapping_add(6) && !self.wx_just_changed;
        }
        if activate {
            self.window_y = self.window_y.wrapping_add(1);
            self.window_tile_x = 0;
            self.bg_fifo.clear();
            self.wx_triggered = true;
            self.fetcher_state = FETCH_TILE_T1;
            self.window_is_being_fetched = true;
            return self.wx == 0 && self.scx & 7 != 0 && !cgb;
        }
        if !cgb && self.wx == 166 && self.wx == position.wrapping_add(7) {
            self.window_y = self.window_y.wrapping_add(1);
        }
        false
    }

    fn object_line_address(&self) -> u16 {
        let height_16 = self.lcdc & LCDC_OBJ_SIZE != 0;
        let mask = if height_16 { 0x0F } else { 0x07 };
        let y = self.objects_y[self.n_visible_objs - 1];
        let mut tile_y = self.ly.wrapping_sub(y) & mask;
        if self.object_flags & 0x40 != 0 {
            tile_y ^= mask;
        }
        let tile = if height_16 {
            self.object_tile & 0xFE
        } else {
            self.object_tile
        };
        let mut address = tile as u16 * 0x10 + tile_y as u16 * 2;
        if self.cgb && self.object_flags & 0x08 != 0 {
            address += 0x2000;
        }
        address
    }

    fn render_pixel(&mut self) {
        if self.n_visible_objs != 0
            && self.objects_enabled()
            && self.objects_x[self.n_visible_objs - 1] == 0
        {
            return;
        }
        if self.bg_fifo.size == 0 {
            return;
        }
        let background = if std::mem::take(&mut self.insert_bg_pixel) {
            FifoItem::default()
        } else {
            self.bg_fifo.pop()
        };
        let mut bg_priority = background.bg_priority;
        let mut object = None;
        if self.obj_fifo.size != 0 {
            let item = self.obj_fifo.pop();
            if item.pixel != 0 && self.lcdc & LCDC_OBJ_ENABLE != 0 {
                bg_priority |= item.bg_priority;
                object = Some(item);
            }
        }

        let position = self.position_in_line;
        if position.wrapping_add(16) < 8 {
            if position == 0u8.wrapping_sub(17) {
                self.position_in_line = POSITION_START;
            } else if position & 7 == self.scx & 7 {
                self.position_in_line = 0u8.wrapping_sub(8);
            } else if self.window_is_being_fetched && position & 7 == 6 && self.scx & 7 == 7 {
                self.position_in_line = 0u8.wrapping_sub(8);
            } else if position == 0u8.wrapping_sub(9) {
                self.position_in_line = POSITION_START;
                return;
            } else {
                self.line_has_fractional_scrolling = true;
            }
        }
        self.window_is_being_fetched = false;

        if self.position_in_line >= SCREEN_WIDTH as u8 {
            self.position_in_line = self.position_in_line.wrapping_add(1);
            return;
        }

        let mut bg_enabled = true;
        if self.lcdc & LCDC_BG_ENABLE == 0 {
            if self.cgb {
                bg_priority = false;
            } else {
                bg_enabled = false;
            }
        }
        let bg_pixel = if bg_enabled { background.pixel } else { 0 };
        if bg_pixel != 0 && bg_priority {
            object = None;
        }
        let color = match object {
            Some(item) if self.cgb => palette_color(&self.obj_palettes, item.palette, item.pixel),
            Some(item) => {
                let palette = if item.palette != 0 {
                    self.obp1
                } else {
                    self.obp0
                };
                let shade = (palette >> (item.pixel * 2)) & 3;
                if self.compat {
                    palette_color(&self.obj_palettes, item.palette, shade)
                } else {
                    shade as u16
                }
            }
            None if self.cgb => palette_color(&self.bg_palettes, background.palette, bg_pixel),
            None => {
                let shade = (self.bgp >> (bg_pixel * 2)) & 3;
                if self.compat {
                    palette_color(&self.bg_palettes, 0, shade)
                } else {
                    shade as u16
                }
            }
        };
        if self.ly < SCREEN_HEIGHT as u8 && (self.lcd_x as usize) < SCREEN_WIDTH {
            self.frame[self.ly as usize * SCREEN_WIDTH + self.lcd_x as usize] = color;
        }
        self.position_in_line = self.position_in_line.wrapping_add(1);
        self.lcd_x = self.lcd_x.wrapping_add(1);
    }

    fn fetcher_row(&self) -> u8 {
        if self.wx_triggered {
            self.window_y
        } else {
            self.ly.wrapping_add(self.scy)
        }
    }

    fn update_wx_glitch(&mut self) {
        if !self.renders_color() {
            return;
        }
        if self.lcdc & LCDC_WIN_ENABLE == 0 || !self.wy_triggered {
            self.cgb_wx_glitch = false;
            return;
        }
        if self.wx == 0 {
            let position = self.position_in_line;
            self.cgb_wx_glitch = position.wrapping_add(16) <= 8
                || position == 0u8.wrapping_sub(7) && self.line_has_fractional_scrolling;
            return;
        }
        self.cgb_wx_glitch = self
            .position_in_line
            .wrapping_add(7)
            .wrapping_add(self.window_is_being_fetched as u8)
            == self.wx;
    }

    fn tile_data_address(&self, y: u8) -> u16 {
        let mut address = if self.lcdc & LCDC_TILE_SEL != 0 {
            self.current_tile as u16 * 0x10
        } else {
            (0x1000 + self.current_tile as i8 as i32 * 0x10) as u16
        };
        if self.current_tile_attributes & 0x08 != 0 {
            address += 0x2000;
        }
        let flip = if self.current_tile_attributes & 0x40 != 0 {
            7
        } else {
            0
        };
        address + ((y & 7) ^ flip) as u16 * 2
    }

    fn fetcher_data_row(&self) -> u8 {
        if self.renders_color() {
            self.fetcher_y
        } else {
            self.fetcher_row()
        }
    }

    fn advance_fetcher(&mut self) {
        match self.fetcher_state {
            FETCH_TILE_T1 => {
                self.update_wx_glitch();
                if self.lcdc & LCDC_WIN_ENABLE == 0 {
                    self.wx_triggered = false;
                }
                let map_bit = if self.wx_triggered {
                    LCDC_WIN_MAP
                } else {
                    LCDC_BG_MAP
                };
                let map = if self.lcdc & map_bit != 0 {
                    0x1C00
                } else {
                    0x1800
                };
                let y = self.fetcher_row();
                let x = if self.wx_triggered {
                    self.window_tile_x
                } else if self.position_in_line.wrapping_add(16) < 8 {
                    self.scx >> 3
                } else {
                    let early = (self.renders_color() && !self.during_object_fetch) as u8;
                    (self
                        .scx
                        .wrapping_add(self.position_in_line)
                        .wrapping_add(8)
                        .wrapping_sub(early)
                        / 8)
                        & 0x1F
                };
                self.fetcher_y = y;
                self.last_tile_index_address = map + x as u16 + (y / 8) as u16 * 32;
                self.fetcher_state += 1;
            }
            FETCH_TILE_T2 => {
                if !self.cgb_wx_glitch {
                    let address = self.last_tile_index_address as usize;
                    self.current_tile = self.vram[address];
                    self.current_tile_attributes = if self.cgb {
                        self.vram[address + 0x2000]
                    } else {
                        0
                    };
                }
                self.fetcher_state += 1;
            }
            FETCH_LOW_T1 => {
                self.update_wx_glitch();
                self.last_tile_data_address = self.tile_data_address(self.fetcher_data_row());
                self.fetcher_state += 1;
            }
            FETCH_LOW_T2 => {
                self.current_tile_data[0] = if self.cgb_wx_glitch {
                    self.current_tile_data[1]
                } else {
                    self.vram[self.last_tile_data_address as usize]
                };
                self.fetcher_state += 1;
            }
            FETCH_HIGH_T1 => {
                self.update_wx_glitch();
                self.last_tile_data_address = self.tile_data_address(self.fetcher_data_row()) + 1;
                self.fetcher_state += 1;
            }
            FETCH_HIGH_T2 => {
                let glitch = self.cgb_wx_glitch;
                self.current_tile_data[1] = if glitch {
                    self.current_tile_data[0]
                } else {
                    self.vram[self.last_tile_data_address as usize]
                };
                if self.wx_triggered {
                    self.window_tile_x = (self.window_tile_x + 1) & 0x1F;
                }
                self.fetcher_state = FETCH_PUSH;
                if !glitch {
                    self.push_fetched_tile();
                }
            }
            _ => self.push_fetched_tile(),
        }
    }

    fn push_fetched_tile(&mut self) {
        if self.bg_fifo.size > 0 {
            return;
        }
        if self.wy_triggered
            && self.lcdc & LCDC_WIN_ENABLE == 0
            && !self.renders_color()
            && !self.disable_window_pixel_insertion_glitch
        {
            let mut logical_position = self.position_in_line.wrapping_add(7);
            if logical_position > 167 {
                logical_position = 0;
            }
            if self.wx == logical_position {
                self.bg_fifo.read_end = self.bg_fifo.read_end.wrapping_sub(1) & 7;
                self.bg_fifo.items[self.bg_fifo.read_end as usize] = FifoItem::default();
                self.bg_fifo.size = 1;
                return;
            }
        }
        let attributes = self.current_tile_attributes;
        let [lower, upper] = self.current_tile_data;
        self.bg_fifo.push_bg_row(
            lower,
            upper,
            attributes & 0x07,
            attributes & 0x80 != 0,
            attributes & 0x20 != 0,
        );
        self.fetcher_state = FETCH_TILE_T1;
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

    fn dots_until_hblank(&self) -> u32 {
        let earliest = OAM_SCAN_DOTS + MIN_TRANSFER_DOTS;
        let first_line_earliest = FIRST_LINE_TRANSFER_DOT + MIN_TRANSFER_DOTS;
        match self.mode {
            MODE_TRANSFER => earliest.saturating_sub(self.dot).max(1),
            MODE_OAM_SCAN => earliest - self.dot,
            MODE_HBLANK if self.stat_mode != MODE_HBLANK => 1,
            MODE_HBLANK if self.first_line && self.dot < FIRST_LINE_TRANSFER_DOT => {
                first_line_earliest - self.dot
            }
            _ => self.dots_until(self.next_visible_line(), earliest),
        }
    }

    /// Dots until mode 0 could next begin on a visible line. It may be early,
    /// never late.
    pub fn t_cycles_until_hblank(&self) -> usize {
        if self.enabled() {
            self.dots_until_hblank() as usize
        } else {
            usize::MAX
        }
    }

    /// T-cycles until the PPU could next raise one of `enabled` interrupts.
    /// It may be early, never late.
    pub fn t_cycles_until_interrupt(&self, enabled: u8) -> usize {
        if !self.enabled() {
            return usize::MAX;
        }
        if self.dmg_mode_change_pending() || self.stat_mode != self.visible_mode {
            return 1;
        }
        let stat = if enabled & STAT_INTERRUPT != 0 {
            self.stat
        } else {
            0
        };
        let mut dots = u32::MAX;
        if enabled & VBLANK_INTERRUPT != 0 || stat & (STAT_VBLANK | STAT_OAM) != 0 {
            let vblank = SCREEN_HEIGHT as u8;
            dots = dots.min(self.dots_until(vblank, self.cpu_visible_dot(vblank)));
        }
        if stat & STAT_OAM != 0 && self.renders_color() {
            dots =
                dots.min(self.dots_until(SCREEN_HEIGHT as u8 - 1, LINE_DOTS - CGB_VBLANK_OAM_LEAD));
        }
        if stat & STAT_HBLANK != 0 {
            dots = dots.min(self.dots_until_hblank());
        }
        if stat & STAT_OAM != 0 {
            let line = self.next_visible_line();
            dots = dots.min(self.dots_until(line, self.cpu_visible_dot(line)));
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
        self.vram[self.vram_bank * 0x2000 + (addr as usize & 0x1FFF)]
    }

    pub fn write_vram(&mut self, addr: u16, value: u8) {
        self.vram[self.vram_bank * 0x2000 + (addr as usize & 0x1FFF)] = value;
    }

    fn palettes_accessible(&self) -> bool {
        self.mode != MODE_TRANSFER && self.visible_mode != MODE_TRANSFER
    }

    pub fn read_cgb_register(&self, addr: u16) -> u8 {
        if !self.cgb {
            return 0xFF;
        }
        match addr {
            0xFF4F => 0xFE | self.vram_bank as u8,
            0xFF68 => 0x40 | self.bg_palette_index,
            0xFF6A => 0x40 | self.obj_palette_index,
            0xFF69 | 0xFF6B if !self.palettes_accessible() => 0xFF,
            0xFF69 => self.bg_palettes[(self.bg_palette_index & 0x3F) as usize],
            0xFF6B => self.obj_palettes[(self.obj_palette_index & 0x3F) as usize],
            0xFF6C => 0xFE | self.x_priority as u8,
            _ => 0xFF,
        }
    }

    pub fn write_cgb_register(&mut self, addr: u16, value: u8) {
        if !self.cgb {
            return;
        }
        let accessible = self.palettes_accessible();
        let write_palette = |palettes: &mut [u8; 64], index: &mut u8| {
            if accessible {
                palettes[(*index & 0x3F) as usize] = value;
            }
            if *index & 0x80 != 0 {
                *index = 0x80 | (*index + 1) & 0x3F;
            }
        };
        match addr {
            0xFF4F => self.vram_bank = (value & 1) as usize,
            0xFF68 => self.bg_palette_index = value & 0xBF,
            0xFF6A => self.obj_palette_index = value & 0xBF,
            0xFF69 => write_palette(&mut self.bg_palettes, &mut self.bg_palette_index),
            0xFF6B => write_palette(&mut self.obj_palettes, &mut self.obj_palette_index),
            0xFF6C => self.x_priority = value & 1 != 0,
            _ => {}
        }
    }

    /// The 8-byte OAM row the DMG's OAM scan was reading `t_ago` dots ago,
    /// when that row can be corrupted (row 0 never is).
    fn oam_bug_row(&self, t_ago: u32) -> Option<usize> {
        if self.cgb || !self.enabled() || self.first_line || self.ly >= SCREEN_HEIGHT as u8 {
            return None;
        }
        let dot = self.dot.checked_sub(t_ago)?;
        let row = ((dot + OAM_BUG_ROW_LAG) / 4) as usize;
        (dot < OAM_SCAN_DOTS && (1..20).contains(&row)).then_some(row * 8)
    }

    fn oam_word(&self, offset: usize) -> u16 {
        u16::from_le_bytes([self.oam[offset], self.oam[offset + 1]])
    }

    fn set_oam_word(&mut self, offset: usize, value: u16) {
        self.oam[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
    }

    fn copy_oam_row(&mut self, from: usize, to: usize) {
        self.oam.copy_within(from..from + 8, to);
    }

    /// Corruption from a write, or from a 16-bit increment or decrement, with
    /// an address in FE00-FEFF during the DMG's OAM scan.
    pub fn oam_bug_write(&mut self, t_ago: u32) {
        let Some(row) = self.oam_bug_row(t_ago) else {
            return;
        };
        let (a, b, c) = (
            self.oam_word(row),
            self.oam_word(row - 8),
            self.oam_word(row - 4),
        );
        self.set_oam_word(row, ((a ^ c) & (b ^ c)) ^ c);
        self.oam.copy_within(row - 6..row, row + 2);
    }

    /// Corruption from a read in FE00-FEFF during the DMG's OAM scan.
    pub fn oam_bug_read(&mut self, t_ago: u32) {
        let Some(row) = self.oam_bug_row(t_ago) else {
            return;
        };
        let word = |ppu: &Ppu, offset: isize| ppu.oam_word(row.wrapping_add_signed(offset));
        let previous = row - 8;
        match row & 0x18 {
            0x10 => {
                let (a, b, c, d) = (
                    word(self, -16),
                    word(self, -8),
                    word(self, 0),
                    word(self, -4),
                );
                self.set_oam_word(previous, (b & (a | c | d)) | (a & c & d));
                self.copy_oam_row(previous, row - 16);
            }
            0x00 => {
                let (a, b, c, d, e) = (
                    word(self, 0),
                    word(self, -4),
                    word(self, -8),
                    word(self, -16),
                    word(self, -32),
                );
                let value = match row {
                    0x40 => {
                        let (f, g) = (word(self, -6), word(self, -14));
                        (c & (e | d | (!f & g) | b | a)) | (b & d & e)
                    }
                    0x20 => (c & (a | b | d | e)) | (a & b & d & e),
                    0x60 => (c & (a | b | d | e)) | (b & d & e),
                    _ => c | (a & b & d & e),
                };
                self.set_oam_word(previous, value);
                self.copy_oam_row(previous, row - 16);
                self.copy_oam_row(previous, row - 32);
            }
            _ => {
                let (a, b, c) = (word(self, 0), word(self, -8), word(self, -4));
                let value = b | (a & c);
                self.set_oam_word(previous, value);
                self.set_oam_word(row, value);
            }
        }
        self.copy_oam_row(previous, row);
        if row == 0x80 {
            self.copy_oam_row(row, 0);
        }
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

    /// The LCDC value the DMG's pixel pipeline sees in the T-cycle before a
    /// write fully lands. Tile data, map and object size selects switch
    /// early, as does turning the window off; at the very start of a line
    /// the background enable switches early too, and objects turn off early
    /// there or while one is being fetched.
    pub fn dmg_lcdc_transition(&self, old: u8, value: u8) -> u8 {
        let early = LCDC_TILE_SEL | LCDC_BG_MAP | LCDC_WIN_MAP | LCDC_OBJ_SIZE;
        let mut lcdc = old & !early | value & early;
        lcdc &= value | !LCDC_WIN_ENABLE;
        if self.position_in_line == 0 {
            lcdc = lcdc & !LCDC_BG_ENABLE | value & LCDC_BG_ENABLE;
        }
        if value & LCDC_OBJ_ENABLE == 0 && (self.position_in_line == 0 || self.during_object_fetch)
        {
            lcdc &= !LCDC_OBJ_ENABLE;
        }
        lcdc
    }

    pub fn write_register(&mut self, addr: u16, value: u8) -> u8 {
        match addr {
            0xFF40 => {
                let was_enabled = self.enabled();
                let old = self.lcdc;
                if !self.renders_color()
                    && old & LCDC_OBJ_ENABLE != 0
                    && value & LCDC_OBJ_ENABLE == 0
                    && self.during_object_fetch
                {
                    self.object_fetch_aborted = true;
                    self.pipeline_sleep = 0;
                }
                if old & LCDC_WIN_ENABLE != 0
                    && value & LCDC_WIN_ENABLE == 0
                    && self.window_is_being_fetched
                {
                    self.disable_window_pixel_insertion_glitch = true;
                }
                self.lcdc = value;
                self.wy_check_pending = true;
                if was_enabled && !self.enabled() {
                    self.ly = 0;
                    self.dot = 0;
                    self.mode = MODE_HBLANK;
                    self.visible_mode = MODE_HBLANK;
                    self.stat_mode = MODE_HBLANK;
                    self.wy_triggered = false;
                    let blank = self.blank_color();
                    self.frame.fill(blank);
                } else if !was_enabled && self.enabled() {
                    self.ly = 0;
                    self.dot = LCD_ON_DOT;
                    self.mode = MODE_HBLANK;
                    self.visible_mode = MODE_HBLANK;
                    self.stat_mode = MODE_HBLANK;
                    self.first_line = true;
                    self.n_visible_objs = 0;
                    self.window_y = 0xFF;
                    self.wy_triggered = false;
                    self.position_in_line = POSITION_START;
                    self.line_has_fractional_scrolling = false;
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
            0xFF4A => {
                self.wy = value;
                self.wy_check_pending = true;
            }
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
        let mut ppu = Ppu::new(false, false);
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
        let base = transfer_dots(&[], 0);
        let mut failures = Vec::new();
        for xs in &cases {
            let measured = transfer_dots(xs, 0) - base;
            let expected = expected_penalty(xs, 0);
            if measured != expected {
                failures.push(format!("{xs:?}: measured {measured}, expected {expected}"));
            }
        }
        assert!(failures.is_empty(), "{}", failures.join("\n"));
    }
}
