#![crate_name = "yargb"]

extern crate num;
extern crate sdl2;

mod cpu;
mod gpu;
mod mbc;
mod mmu;
mod registers;
mod timer;
mod utils;

use crate::cpu::CPU;
use crate::cpu::jit::Jit;
use std::fs::File;
use std::io::prelude::*;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;

fn main() {
    let mut rom_path = String::from("roms/cpu_instrs.gb");
    let mut interpreter = false;
    let mut trace = false;
    let mut max_cycles = None;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--interpreter" => interpreter = true,
            "--trace" => trace = true,
            "--max-cycles" => {
                max_cycles = Some(
                    args.next()
                        .and_then(|value| value.parse::<usize>().ok())
                        .expect("--max-cycles needs a number"),
                )
            }
            path => rom_path = path.to_string(),
        }
    }

    let mut rom_file = File::open(&rom_path).expect("failed to open rom file");
    let mut rom_data = vec![0; 0];

    rom_file
        .read_to_end(&mut rom_data)
        .expect("failed to read rom file");

    let mut cpu = CPU::new(&rom_data);
    cpu.trace = trace;
    let mut jit = (!interpreter && !trace).then(|| Jit::new(rom_data.len()));
    let mut cycles = 0;

    // let sdl_context = sdl2::init().unwrap();
    // let video_subsystem = sdl_context.video().unwrap();

    // let window = video_subsystem
    //     .window("yargb", 160, 144)
    //     .position_centered()
    //     .build()
    //     .unwrap();

    // let mut canvas = window.into_canvas().build().unwrap();

    // canvas.set_draw_color(Color::RGB(0, 255, 255));
    // canvas.clear();
    // canvas.present();

    let mut running = true;
    // let mut event_pump = sdl_context.event_pump().unwrap();
    // let mut i = 0;

    while running && max_cycles.is_none_or(|max| cycles < max) {
        cycles += match &mut jit {
            Some(jit) => jit.step(&mut cpu).ticks,
            None => cpu.cycle(),
        };

        // i = (i + 1) % 255;
        // canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
        // canvas.clear();
        // for event in event_pump.poll_iter() {
        //     match event {
        //         Event::Quit { .. }
        //         | Event::KeyDown {
        //             keycode: Some(Keycode::Escape),
        //             ..
        //         } => {
        //             running = false;
        //         }
        //         _ => {}
        //     }
        // }

        // canvas.present();
    }

    cpu.print_reg_state();
}
