#![crate_name = "yargb"]

extern crate num;
extern crate glutin;

mod mbc;
mod mmu;
mod cpu;
mod registers;
mod gpu;
mod utils;

use std::io::prelude::*;
use std::fs::File;
use cpu::CPU;

use glutin::GlContext;

fn main() {
    let mut rom_file = File::open("roms/cpu_instrs.gb").expect("failed to open rom, lol");
    let mut rom_data: [u8; 32000] = [0; 32000];

    rom_file.read(&mut rom_data).expect("failed to read rom, lol");

    let mut cpu = CPU::new(&rom_data);

    let mut events_loop = glutin::EventsLoop::new();
    let window = glutin::WindowBuilder::new()
        .with_title("yargb")
        .with_dimensions(160, 144);
    let context = glutin::ContextBuilder::new()
        .with_vsync(true);
    let gl_window = glutin::GlWindow::new(window, context, &events_loop).unwrap();

    unsafe {
        gl_window.make_current().unwrap();
    }

    let mut running = true;

    while running {
        cpu.cycle();

        // events_loop.poll_events(|event| {
        //     match event {
        //         glutin::Event::WindowEvent{ event, .. } => match event {
        //             glutin::WindowEvent::Closed => running = false,
        //             glutin::WindowEvent::Resized(w, h) => gl_window.resize(w, h),
        //             _ => ()
        //         },
        //         _ => ()
        //     }
        // });

        // gl_window.swap_buffers().unwrap();
    }

    cpu.print_reg_state();
}
