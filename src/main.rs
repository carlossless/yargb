#![crate_name = "yargb"]

mod mbc;
mod mmu;
mod cpu;
mod registers;

use std::io;
use std::io::prelude::*;
use std::fs::File;
use cpu::CPU;

fn main() {
	let mut rom_file = File::open("roms/tetris.gb").expect("failed to open tetris, lol");
	let mut rom_data: [u8; 32000] = [0; 32000];

	rom_file.read(&mut rom_data).expect("failed to read tetris, lol");

	let mut cpu = CPU::new(&rom_data);

	while !cpu.halted {
		cpu.cycle();
	}

    cpu.print_reg_state();
}
