macro_rules! load {
    ($target:ident, $source:ident) => {
        |cpu: &mut Cpu| {
            cpu.regs.$target = cpu.regs.$source;
            1
        }
    };
    ($target:ident) => {
        |cpu: &mut Cpu, value| {
            cpu.regs.$target = value;
            2
        }
    };
}
