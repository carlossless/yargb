//! Public test ROM suites, checked against their own pass criteria. The ROMs
//! are not part of the repository: `scripts/test-roms.sh fetch` downloads
//! them into `target/test-roms`, or `YARGB_TEST_ROMS` points elsewhere.

use std::fs;
use std::path::{Path, PathBuf};

use crate::cartridge::Cartridge;
use crate::emulator::Emulator;
use crate::frontend::{SCREENSHOT_SHADES, to_rgb};
use crate::mmu::Model;

pub fn test_rom(path: &str) -> PathBuf {
    let root = std::env::var("YARGB_TEST_ROMS").unwrap_or_else(|_| "target/test-roms".into());
    Path::new(&root).join(path)
}

fn emulator(path: &Path, model: Option<Model>, jit: bool) -> Emulator {
    let rom = fs::read(path).unwrap_or_else(|error| panic!("{}: {error}", path.display()));
    let cartridge = Cartridge::new(rom, None);
    let model = model.unwrap_or_else(|| Model::for_cartridge(&cartridge));
    let mut emulator = Emulator::new(cartridge, model, jit, false);
    emulator.record_serial(true);
    emulator
}

/// Runs until `done` holds for the serial output so far, or `frames` pass.
fn serial_output(emulator: &mut Emulator, frames: usize, done: impl Fn(&[u8]) -> bool) -> Vec<u8> {
    let mut output = Vec::new();
    for _ in 0..frames {
        emulator.run_frame();
        emulator.take_samples();
        output.extend(emulator.take_serial());
        if done(&output) {
            break;
        }
    }
    output
}

fn contains(haystack: &[u8], needle: &[u8]) -> bool {
    haystack
        .windows(needle.len())
        .any(|window| window == needle)
}

fn assert_no_failures(suite: &str, failures: Vec<String>) {
    assert!(failures.is_empty(), "{suite}:\n  {}", failures.join("\n  "));
}

const EXECUTION_MODES: [(bool, &str); 2] = [(true, "JIT"), (false, "interpreter")];

/// Blargg's CPU and memory timing suites report over the serial port.
#[test]
#[ignore = "needs test ROMs"]
fn blargg_serial_suites() {
    let mut failures = Vec::new();
    for model in [Model::Dmg, Model::Cgb] {
        for rom in [
            "cpu_instrs/cpu_instrs.gb",
            "instr_timing/instr_timing.gb",
            "mem_timing/mem_timing.gb",
        ] {
            for (jit, mode) in EXECUTION_MODES {
                let mut emulator = emulator(&test_rom(&format!("blargg/{rom}")), Some(model), jit);
                let output = serial_output(&mut emulator, 4000, |output| {
                    contains(output, b"Passed") || contains(output, b"Failed")
                });
                if !contains(&output, b"Passed") {
                    failures.push(format!("{rom} ({model:?}, {mode})"));
                }
            }
        }
    }
    assert_no_failures("blargg", failures);
}

/// Blargg's other suites leave their result in cartridge RAM: a signature at
/// $A001-$A003 and the result code, 0 for a pass, at $A000.
#[test]
#[ignore = "needs test ROMs"]
fn blargg_memory_suites() {
    let mut failures = Vec::new();
    for (rom, model) in [
        ("mem_timing-2/mem_timing.gb", Model::Dmg),
        ("halt_bug.gb", Model::Dmg),
        ("dmg_sound/dmg_sound.gb", Model::Dmg),
        ("oam_bug/oam_bug.gb", Model::Dmg),
        ("cgb_sound/cgb_sound.gb", Model::Cgb),
        ("interrupt_time/interrupt_time.gb", Model::Cgb),
    ] {
        for (jit, mode) in EXECUTION_MODES {
            let mut emulator = emulator(&test_rom(&format!("blargg/{rom}")), Some(model), jit);
            let mut result = None;
            for _ in 0..4000 {
                emulator.run_frame();
                emulator.take_samples();
                let ram = emulator.cartridge_ram();
                if ram.len() >= 4 && ram[1..4] == [0xDE, 0xB0, 0x61] && ram[0] != 0x80 {
                    result = Some(ram[0]);
                    break;
                }
            }
            if result != Some(0) {
                failures.push(format!("{rom} ({mode}): result {result:?}"));
            }
        }
    }
    assert_no_failures("blargg", failures);
}

/// Mooneye tests pass by sending the Fibonacci numbers over serial. File name
/// suffixes name the models a test is for.
fn mooneye_applies(name: &str, model: Model) -> bool {
    let (endings, infixes): (&[&str], &[&str]) = match model {
        Model::Dmg => (
            &["-dmg0", "-mgb", "-sgb", "-sgb2", "-S", "-C", "-A"],
            &["-cgb", "-agb", "-ags"],
        ),
        Model::Cgb => (
            &["-mgb", "-sgb", "-sgb2", "-S", "-G", "-GS", "-A", "-cgb0"],
            &["-dmg", "-agb", "-ags"],
        ),
    };
    !endings.iter().any(|ending| name.ends_with(ending))
        && !infixes.iter().any(|infix| name.contains(infix))
}

fn mooneye_roms(model: Model) -> Vec<PathBuf> {
    let mut directories = vec![test_rom("mooneye/acceptance")];
    if model == Model::Cgb {
        directories.push(test_rom("mooneye/misc"));
    }
    let mut roms = Vec::new();
    while let Some(directory) = directories.pop() {
        for entry in fs::read_dir(&directory).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                directories.push(path);
            } else if path.extension().is_some_and(|extension| extension == "gb")
                && mooneye_applies(path.file_stem().unwrap().to_str().unwrap(), model)
            {
                roms.push(path);
            }
        }
    }
    roms.sort();
    roms
}

#[test]
#[ignore = "needs test ROMs"]
fn mooneye_acceptance() {
    const PASS: &[u8] = &[3, 5, 8, 13, 21, 34];
    let mut failures = Vec::new();
    for model in [Model::Dmg, Model::Cgb] {
        let roms = mooneye_roms(model);
        assert!(!roms.is_empty(), "no mooneye ROMs found");
        for rom in roms {
            for (jit, mode) in EXECUTION_MODES {
                let mut emulator = emulator(&rom, Some(model), jit);
                let output = serial_output(&mut emulator, 600, |output| contains(output, PASS));
                if !contains(&output, PASS) {
                    failures.push(format!("{} ({model:?}, {mode})", rom.display()));
                }
            }
        }
    }
    assert_no_failures("mooneye", failures);
}

/// Decodes a reference PNG to 8-bit RGB.
fn reference_image(path: &Path) -> Vec<u8> {
    let file = fs::File::open(path).unwrap_or_else(|error| panic!("{}: {error}", path.display()));
    let mut decoder = png::Decoder::new(std::io::BufReader::new(file));
    decoder.set_transformations(png::Transformations::EXPAND | png::Transformations::STRIP_16);
    let mut reader = decoder.read_info().unwrap();
    let mut buffer = vec![0; reader.output_buffer_size().unwrap()];
    let info = reader.next_frame(&mut buffer).unwrap();
    let pixels = &buffer[..info.buffer_size()];
    match info.color_type {
        png::ColorType::Grayscale => pixels.iter().flat_map(|&gray| [gray; 3]).collect(),
        png::ColorType::GrayscaleAlpha => {
            pixels.chunks(2).flat_map(|pixel| [pixel[0]; 3]).collect()
        }
        png::ColorType::Rgb => pixels.to_vec(),
        png::ColorType::Rgba => pixels
            .chunks(4)
            .flat_map(|pixel| [pixel[0], pixel[1], pixel[2]])
            .collect(),
        other => panic!("{}: unexpected colour type {other:?}", path.display()),
    }
}

/// Runs a ROM for `frames` and returns how many pixels differ from the
/// reference image.
fn screen_difference(rom: &Path, reference: &Path, frames: usize, jit: bool) -> usize {
    let mut emulator = emulator(rom, None, jit);
    for _ in 0..frames {
        emulator.run_frame();
        emulator.take_samples();
    }
    let screen = to_rgb(&emulator, &SCREENSHOT_SHADES);
    let expected = reference_image(reference);
    assert_eq!(
        screen.len(),
        expected.len(),
        "{}: image size",
        reference.display()
    );
    screen
        .chunks(3)
        .zip(expected.chunks(3))
        .filter(|(ours, theirs)| ours != theirs)
        .count()
}

#[test]
#[ignore = "needs test ROMs"]
fn acid2() {
    let mut failures = Vec::new();
    for (rom, reference) in [
        ("acid/dmg-acid2.gb", "acid/dmg-acid2.png"),
        ("acid/cgb-acid2.gbc", "acid/cgb-acid2.png"),
    ] {
        for (jit, mode) in EXECUTION_MODES {
            let difference = screen_difference(&test_rom(rom), &test_rom(reference), 120, jit);
            if difference != 0 {
                failures.push(format!("{rom} ({mode}): {difference} pixels differ"));
            }
        }
    }
    assert_no_failures("acid2", failures);
}

/// Mealybug Tearoom's expected images were captured on a DMG.
#[test]
#[ignore = "needs test ROMs"]
fn mealybug_tearoom() {
    let mut failures = Vec::new();
    let mut references: Vec<PathBuf> = fs::read_dir(test_rom("mealybug/expected"))
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .collect();
    references.sort();
    assert!(!references.is_empty(), "no mealybug references found");
    for reference in references {
        let name = reference.file_stem().unwrap().to_str().unwrap();
        let rom = test_rom(&format!("mealybug/roms/{name}.gb"));
        for (jit, mode) in EXECUTION_MODES {
            let difference = screen_difference(&rom, &reference, 60, jit);
            if difference != 0 {
                failures.push(format!("{name} ({mode}): {difference} pixels differ"));
            }
        }
    }
    assert_no_failures("mealybug", failures);
}
