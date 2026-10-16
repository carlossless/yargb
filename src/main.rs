#![crate_name = "yargb"]

mod apu;
mod cartridge;
mod cpu;
mod emulator;
mod frontend;
mod mmu;
mod ppu;
mod registers;
#[cfg(test)]
mod test_roms;
mod timer;

use std::path::PathBuf;
use std::process::ExitCode;

use crate::cartridge::Cartridge;
use crate::emulator::Emulator;
use crate::mmu::Model;

const USAGE: &str = "usage: yargb [options] ROM

options:
  --headless         run without a window or audio
  --frames N         stop after N frames
  --screenshot PATH  write the last frame to PATH as a PNG
  --scale N          window scale (default 4)
  --serial           print bytes sent over the link cable to stderr
  --model MODEL      dmg, cgb, or auto to pick from the cartridge header (default)
  --interpreter      run without the JIT
  --trace            print every instruction (implies --interpreter)";

struct Options {
    rom: PathBuf,
    headless: bool,
    frames: Option<u64>,
    screenshot: Option<PathBuf>,
    scale: u32,
    serial: bool,
    interpreter: bool,
    model: Option<Model>,
    trace: bool,
}

fn parse_options() -> Result<Options, String> {
    let mut rom = None;
    let mut options = Options {
        rom: PathBuf::new(),
        headless: false,
        frames: None,
        screenshot: None,
        scale: 4,
        serial: false,
        interpreter: false,
        model: None,
        trace: false,
    };
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        let mut value = |name: &str| args.next().ok_or(format!("{name} needs a value"));
        match arg.as_str() {
            "--headless" => options.headless = true,
            "--frames" => {
                options.frames = Some(
                    value("--frames")?
                        .parse()
                        .map_err(|_| "--frames needs a number")?,
                )
            }
            "--screenshot" => options.screenshot = Some(value("--screenshot")?.into()),
            "--scale" => {
                options.scale = value("--scale")?
                    .parse()
                    .map_err(|_| "--scale needs a number")?
            }
            "--serial" => options.serial = true,
            "--interpreter" => options.interpreter = true,
            "--model" => {
                options.model = match value("--model")?.as_str() {
                    "dmg" => Some(Model::Dmg),
                    "cgb" => Some(Model::Cgb),
                    "auto" => None,
                    other => return Err(format!("unknown model {other}")),
                }
            }
            "--trace" => options.trace = true,
            flag if flag.starts_with("--") => return Err(format!("unknown option {flag}")),
            path => rom = Some(PathBuf::from(path)),
        }
    }
    options.rom = rom.ok_or("missing ROM path")?;
    if options.headless && options.frames.is_none() {
        return Err("--headless needs --frames".into());
    }
    Ok(options)
}

fn run(options: Options) -> Result<(), String> {
    let rom = std::fs::read(&options.rom)
        .map_err(|error| format!("failed to read {}: {error}", options.rom.display()))?;
    let cartridge = Cartridge::new(rom, Some(options.rom.with_extension("sav")));
    let model = options
        .model
        .unwrap_or_else(|| Model::for_cartridge(&cartridge));
    let mut emulator = Emulator::new(cartridge, model, !options.interpreter, options.trace);
    emulator.record_serial(options.serial);

    if options.headless {
        for _ in 0..options.frames.unwrap_or(0) {
            emulator.run_frame();
            emulator.take_samples();
            frontend::print_serial(&mut emulator);
        }
    } else {
        frontend::run_window(&mut emulator, options.scale, options.frames)?;
    }

    if let Some(path) = &options.screenshot {
        frontend::write_screenshot(&emulator, path)?;
    }
    emulator
        .save()
        .map_err(|error| format!("failed to write save file: {error}"))
}

fn main() -> ExitCode {
    match parse_options().and_then(run) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}\n\n{USAGE}");
            ExitCode::FAILURE
        }
    }
}
