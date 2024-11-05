use cpu::CPU;
// use registers::Flag::{C, H, N, Z};

macro_rules! load {
    ($source_register:ident, $target_register:ident) => {
        |cpu: &mut CPU| {
            cpu.regs.$source_register = cpu.regs.$target_register;
            1
        }
    };
    ($source_register:ident) => {
        |cpu: &mut CPU, value| {
            cpu.regs.$source_register = value;
            2
        }
    };
}

impl CPU {}
