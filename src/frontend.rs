use std::fs::File;
use std::io::BufWriter;
use std::path::Path;
use std::thread::sleep;
use std::time::{Duration, Instant};

use sdl2::audio::AudioSpecDesired;
use sdl2::event::Event;
use sdl2::keyboard::{Keycode, Scancode};
use sdl2::pixels::PixelFormatEnum;

use crate::apu::SAMPLE_RATE;
use crate::emulator::{Emulator, FRAMES_PER_SECOND};
use crate::mmu::buttons;
use crate::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};

const WINDOW_SHADES: [[u8; 3]; 4] = [
    [0xE0, 0xF8, 0xD0],
    [0x88, 0xC0, 0x70],
    [0x34, 0x68, 0x56],
    [0x08, 0x18, 0x20],
];
const SCREENSHOT_SHADES: [[u8; 3]; 4] = [
    [0xFF, 0xFF, 0xFF],
    [0xAA, 0xAA, 0xAA],
    [0x55, 0x55, 0x55],
    [0x00, 0x00, 0x00],
];
const QUEUED_AUDIO_FRAMES: f64 = 4.0;

const KEYMAP: [(Scancode, u8); 10] = [
    (Scancode::Right, buttons::RIGHT),
    (Scancode::Left, buttons::LEFT),
    (Scancode::Up, buttons::UP),
    (Scancode::Down, buttons::DOWN),
    (Scancode::Z, buttons::A),
    (Scancode::X, buttons::B),
    (Scancode::Return, buttons::START),
    (Scancode::Backspace, buttons::SELECT),
    (Scancode::RShift, buttons::SELECT),
    (Scancode::Space, buttons::START),
];

/// Converts a frame to RGB888: DMG shades through `shades`, CGB colours by
/// widening each 5-bit channel.
fn to_rgb(emulator: &Emulator, shades: &[[u8; 3]; 4]) -> Vec<u8> {
    let frame = emulator.frame().iter();
    if emulator.renders_color() {
        let widen = |channel: u16| ((channel << 3) | (channel >> 2)) as u8;
        frame
            .flat_map(|&color| [color & 0x1F, (color >> 5) & 0x1F, (color >> 10) & 0x1F].map(widen))
            .collect()
    } else {
        frame.flat_map(|&shade| shades[shade as usize]).collect()
    }
}

pub fn write_screenshot(emulator: &Emulator, path: &Path) -> Result<(), String> {
    let file = File::create(path).map_err(|error| error.to_string())?;
    let mut encoder = png::Encoder::new(
        BufWriter::new(file),
        SCREEN_WIDTH as u32,
        SCREEN_HEIGHT as u32,
    );
    encoder.set_color(png::ColorType::Rgb);
    encoder.set_depth(png::BitDepth::Eight);
    let mut writer = encoder.write_header().map_err(|error| error.to_string())?;
    writer
        .write_image_data(&to_rgb(emulator, &SCREENSHOT_SHADES))
        .map_err(|error| error.to_string())
}

/// Runs the emulator in a window until it is closed or `frames` have passed.
/// Speed is paced by the audio queue, or by the clock when there is no audio
/// device. Holding Tab runs unthrottled.
pub fn run_window(emulator: &mut Emulator, scale: u32, frames: Option<u64>) -> Result<(), String> {
    let sdl = sdl2::init()?;
    let video = sdl.video()?;
    let window = video
        .window(
            "yargb",
            SCREEN_WIDTH as u32 * scale,
            SCREEN_HEIGHT as u32 * scale,
        )
        .position_centered()
        .resizable()
        .build()
        .map_err(|error| error.to_string())?;
    let mut canvas = window
        .into_canvas()
        .build()
        .map_err(|error| error.to_string())?;
    canvas
        .set_logical_size(SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32)
        .map_err(|error| error.to_string())?;
    let texture_creator = canvas.texture_creator();
    let mut texture = texture_creator
        .create_texture_streaming(
            PixelFormatEnum::RGB24,
            SCREEN_WIDTH as u32,
            SCREEN_HEIGHT as u32,
        )
        .map_err(|error| error.to_string())?;

    let audio = sdl.audio().ok().and_then(|audio| {
        let spec = AudioSpecDesired {
            freq: Some(SAMPLE_RATE as i32),
            channels: Some(2),
            samples: Some(512),
        };
        audio.open_queue::<f32, _>(None, &spec).ok()
    });
    if let Some(queue) = &audio {
        queue.resume();
    }
    let queued_audio_limit =
        (SAMPLE_RATE as f64 / FRAMES_PER_SECOND * QUEUED_AUDIO_FRAMES) as u32 * 2 * 4;

    let frame_duration = Duration::from_secs_f64(1.0 / FRAMES_PER_SECOND);
    let mut deadline = Instant::now();
    let mut events = sdl.event_pump()?;
    let mut frame = 0;
    while frames.is_none_or(|frames| frame < frames) {
        for event in events.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => return Ok(()),
                _ => {}
            }
        }
        let keyboard = events.keyboard_state();
        let pressed = KEYMAP
            .iter()
            .filter(|(scancode, _)| keyboard.is_scancode_pressed(*scancode))
            .fold(0, |pressed, (_, button)| pressed | button);
        let fast_forward = keyboard.is_scancode_pressed(Scancode::Tab);

        emulator.set_buttons(pressed);
        emulator.run_frame();
        frame += 1;
        let samples = emulator.take_samples();

        texture
            .update(None, &to_rgb(emulator, &WINDOW_SHADES), SCREEN_WIDTH * 3)
            .map_err(|error| error.to_string())?;
        canvas.clear();
        canvas.copy(&texture, None, None)?;
        canvas.present();

        if fast_forward {
            deadline = Instant::now();
            continue;
        }
        match &audio {
            Some(queue) => {
                queue.queue_audio(&samples)?;
                while queue.size() > queued_audio_limit {
                    sleep(Duration::from_millis(1));
                }
            }
            None => {
                deadline += frame_duration;
                let now = Instant::now();
                if deadline > now {
                    sleep(deadline - now);
                } else if now - deadline > frame_duration * 6 {
                    deadline = now;
                }
            }
        }
    }
    Ok(())
}
