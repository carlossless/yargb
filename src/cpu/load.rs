use cpu::CPU;
use registers::Flag::{ Z, N, H, C };

macro_rules! load {
    ($source_register:ident, $target_register:ident) => (
        |cpu: &mut CPU| {
            cpu.regs.$source_register = cpu.regs.$target_register;
            1
        }
    )
}

impl CPU {
}