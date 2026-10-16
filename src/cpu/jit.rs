use std::mem::{offset_of, transmute};

use cranelift_codegen::Context;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    AbiParam, Block, InstBuilder, MemFlagsData, SigRef, Signature, Type, UserFuncName, Value, types,
};
use cranelift_codegen::isa::TargetFrontendConfig;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Module, default_libcall_names};

use crate::cpu::CPU;
use crate::mmu::{MMU, needs_sync};
use crate::registers::Registers;

const MAX_BLOCK_INSTRUCTIONS: u32 = 64;
const MAX_BLOCK_BYTES: usize = MAX_BLOCK_INSTRUCTIONS as usize * 3;
const MAX_INSTRUCTION_TICKS: i64 = 6;
const HOT_THRESHOLD: u16 = 2048;

const A: usize = 7;
const F: usize = 6;
const H: usize = 4;
const L: usize = 5;

type BlockFn = unsafe extern "C" fn(*mut CPU) -> u64;

#[derive(Clone, Copy)]
enum RomEntry {
    Cold(u16),
    Compiled(BlockFn),
    Interpreted,
}

struct RamBlock {
    code: BlockFn,
    end: usize,
}

pub struct Step {
    pub ticks: usize,
    #[cfg_attr(not(test), allow(dead_code))]
    pub instructions: usize,
}

/// Compiles straight-line runs of Game Boy code into native blocks with Cranelift.
///
/// A block runs until a control transfer or an IO, MBC or code write. It only
/// starts if the timer cannot overflow before it ends; otherwise that step is
/// interpreted, so interrupts are taken on the same instruction as in the
/// interpreter. ROM, WRAM and HRAM blocks are compiled once their start address
/// has been reached `HOT_THRESHOLD` times, and RAM blocks are dropped when their
/// bytes are written. Generated code is never freed, so code that is rewritten
/// and recompiled over and over grows memory use.
pub struct Jit {
    module: JITModule,
    ctx: Context,
    builder_ctx: FunctionBuilderContext,
    rom_blocks: Vec<RomEntry>,
    ram_blocks: Vec<Option<RamBlock>>,
    ram_hits: Vec<u16>,
    hot_threshold: u16,
}

impl Jit {
    pub fn new(cpu: &CPU) -> Jit {
        let mut flags = settings::builder();
        flags.set("opt_level", "speed").unwrap();
        flags.set("use_colocated_libcalls", "false").unwrap();
        flags.set("is_pic", "false").unwrap();
        if !cfg!(debug_assertions) {
            flags.set("enable_verifier", "false").unwrap();
        }
        let isa = cranelift_native::builder()
            .expect("host machine is not supported by the JIT")
            .finish(settings::Flags::new(flags))
            .unwrap();
        let module = JITModule::new(JITBuilder::with_isa(isa, default_libcall_names()));
        Jit {
            ctx: module.make_context(),
            module,
            builder_ctx: FunctionBuilderContext::new(),
            rom_blocks: vec![RomEntry::Cold(0); cpu.mmu.cartridge().rom_len()],
            ram_blocks: (0..0x10000).map(|_| None).collect(),
            ram_hits: vec![0; 0x10000],
            hot_threshold: HOT_THRESHOLD,
        }
    }

    pub fn step(&mut self, cpu: &mut CPU) -> Step {
        if let Some(block) = self.block_at(cpu) {
            cpu.mmu.take_exit_request();
            cpu.jit_ticks = 0;
            cpu.jit_synced = 0;
            cpu.jit_budget = cpu
                .mmu
                .m_cycles_until_event(cpu.regs.ime)
                .min(u32::MAX as usize) as u32;
            let result = unsafe { block(cpu) };
            let instructions = (result >> 32) as usize;
            if instructions > 0 {
                let ticks = result as u32;
                cpu.mmu.tick((ticks - cpu.jit_synced) as usize);
                return Step {
                    ticks: ticks as usize,
                    instructions,
                };
            }
        }
        Step {
            ticks: cpu.cycle(),
            instructions: 1,
        }
    }

    fn block_at(&mut self, cpu: &mut CPU) -> Option<BlockFn> {
        for addr in cpu.mmu.take_dirty_code() {
            self.invalidate(&mut cpu.mmu, addr as usize);
        }
        if cpu.trace
            || cpu.halted
            || cpu.halt_bug
            || cpu.locked
            || cpu.ime_toggle
            || cpu.mmu.dma_active()
            || cpu.stopped
            || cpu.mmu.stall_pending()
            || cpu.regs.ime && cpu.mmu.pending_interrupts() != 0
        {
            return None;
        }
        let pc = cpu.regs.pc;
        if pc < 0x8000 {
            self.rom_block(&cpu.mmu, pc)
        } else {
            self.ram_block(&mut cpu.mmu, pc)
        }
    }

    fn rom_block(&mut self, mmu: &MMU, pc: u16) -> Option<BlockFn> {
        let offset = mmu.rom_offset(pc);
        let entry = match *self.rom_blocks.get(offset)? {
            RomEntry::Cold(hits) if hits < self.hot_threshold => {
                self.rom_blocks[offset] = RomEntry::Cold(hits + 1);
                return None;
            }
            RomEntry::Cold(_) => {
                let entry = match self.compile(mmu, pc) {
                    Some((code, _)) => RomEntry::Compiled(code),
                    None => RomEntry::Interpreted,
                };
                self.rom_blocks[offset] = entry;
                entry
            }
            entry => entry,
        };
        match entry {
            RomEntry::Compiled(code) => Some(code),
            _ => None,
        }
    }

    fn ram_block(&mut self, mmu: &mut MMU, pc: u16) -> Option<BlockFn> {
        let start = pc as usize;
        if let Some(block) = &self.ram_blocks[start] {
            return Some(block.code);
        }
        if self.ram_hits[start] < self.hot_threshold {
            self.ram_hits[start] += 1;
            return None;
        }
        let (code, end) = self.compile(mmu, pc)?;
        mmu.track_code(start..end);
        self.ram_blocks[start] = Some(RamBlock { code, end });
        Some(code)
    }

    fn invalidate(&mut self, mmu: &mut MMU, addr: usize) {
        for start in addr.saturating_sub(MAX_BLOCK_BYTES)..=addr {
            if let Some(block) = &self.ram_blocks[start]
                && block.end > addr
            {
                mmu.untrack_code(start..block.end);
                self.ram_blocks[start] = None;
                self.ram_hits[start] = 0;
            }
        }
    }

    fn compile(&mut self, mmu: &MMU, start: u16) -> Option<(BlockFn, usize)> {
        let end = region_end(start)?;
        if start as u32 + instruction_length(mmu.read_byte(start)) > end {
            return None;
        }

        let ptr = self.module.target_config().pointer_type();
        let mut signature = self.module.make_signature();
        signature.params.push(AbiParam::new(ptr));
        signature.returns.push(AbiParam::new(types::I64));
        let id = self.module.declare_anonymous_function(&signature).unwrap();
        self.ctx.func.signature = signature;
        self.ctx.func.name = UserFuncName::user(0, id.as_u32());

        let sigs = [Vec::new(), vec![types::I32], vec![types::I32, types::I32]].map(|args| {
            let mut sig = self.module.make_signature();
            sig.params.push(AbiParam::new(ptr));
            sig.params.extend(args.into_iter().map(AbiParam::new));
            sig.returns.push(AbiParam::new(types::I32));
            sig
        });
        let builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);
        let config = self.module.target_config();
        let mut emitter = Emitter::new(builder, ptr, sigs);
        emitter.oam_bug = mmu.has_oam_bug();
        let block_end = emitter.emit_block(mmu, start, end, config);

        self.module.define_function(id, &mut self.ctx).unwrap();
        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().unwrap();
        let code = self.module.get_finalized_function(id);
        let code = unsafe { transmute::<*const u8, BlockFn>(code) };
        Some((code, block_end as usize))
    }
}

fn region_end(pc: u16) -> Option<u32> {
    match pc {
        0x0000..=0x3FFF => Some(0x4000),
        0x4000..=0x7FFF => Some(0x8000),
        0xC000..=0xDFFF => Some(0xE000),
        0xFF80..=0xFFFE => Some(0xFFFF),
        _ => None,
    }
}

struct Decoded {
    op: u8,
    pc: u16,
    next: u16,
    imm8: u8,
    imm16: u16,
}

fn decode(mmu: &MMU, start: u16, end: u32) -> Vec<Decoded> {
    let mut instructions = Vec::new();
    let mut pc = start;
    while instructions.len() < MAX_BLOCK_INSTRUCTIONS as usize {
        let op = mmu.read_byte(pc);
        let length = instruction_length(op);
        let next = pc as u32 + length;
        if next > end {
            break;
        }
        instructions.push(Decoded {
            op,
            pc,
            next: next as u16,
            imm8: if length > 1 { mmu.read_byte(pc + 1) } else { 0 },
            imm16: if length > 2 { mmu.read_word(pc + 1) } else { 0 },
        });
        if ends_block(op) {
            break;
        }
        pc = next as u16;
    }
    instructions
}

fn ends_block(op: u8) -> bool {
    matches!(
        op,
        0x10 | 0x18
            | 0x20
            | 0x28
            | 0x30
            | 0x38
            | 0xC0
            | 0xC2
            | 0xC3
            | 0xC4
            | 0xC7
            | 0xC8
            | 0xC9
            | 0xCA
            | 0xCC
            | 0xCD
            | 0xCF
            | 0xD0
            | 0xD2
            | 0xD4
            | 0xD7
            | 0xD8
            | 0xD9
            | 0xDA
            | 0xDC
            | 0xDF
            | 0xE7
            | 0xE9
            | 0xEF
            | 0xF7
            | 0xFB
            | 0xFF
            | 0x76
            | 0xD3
            | 0xDB
            | 0xDD
            | 0xE3
            | 0xE4
            | 0xEB
            | 0xEC
            | 0xED
            | 0xF4
            | 0xFC
            | 0xFD
    )
}

fn instruction_length(op: u8) -> u32 {
    if op == 0xCB {
        2
    } else {
        CPU::OPS[op as usize].size as u32
    }
}

extern "C" fn jit_read(cpu: *mut CPU, addr: u32) -> u32 {
    let cpu = unsafe { &mut *cpu };
    let addr = addr as u16;
    if needs_sync(addr) {
        cpu.sync_jit_hardware();
    }
    cpu.mmu.cpu_read(addr) as u32
}

extern "C" fn jit_write(cpu: *mut CPU, addr: u32, value: u32) -> u32 {
    let cpu = unsafe { &mut *cpu };
    let addr = addr as u16;
    if needs_sync(addr) {
        cpu.jit_ticks -= 1;
        cpu.sync_jit_hardware();
        cpu.mmu.begin_cpu_write(addr, value as u8);
        cpu.jit_synced += 1;
    } else {
        cpu.mmu.cpu_write(addr, value as u8);
    }
    cpu.mmu.take_exit_request() as u32
}

extern "C" fn jit_oam_bug(cpu: *mut CPU, addr: u32) -> u32 {
    let cpu = unsafe { &mut *cpu };
    cpu.sync_jit_hardware();
    cpu.mmu.oam_bug_idle(addr as u16);
    0
}

extern "C" fn jit_interpret(cpu: *mut CPU) -> u32 {
    let cpu = unsafe { &mut *cpu };
    cpu.sync_jit_hardware();
    let ticks = cpu.process() as u32;
    cpu.jit_synced += ticks;
    let exit = cpu.mmu.take_exit_request()
        || cpu.halted
        || cpu.locked
        || cpu.stopped
        || cpu.mmu.dma_active()
        || cpu.mmu.stall_pending();
    ticks | (exit as u32) << 16
}

enum Flag {
    Keep,
    Clear,
    Set,
    Value(Value),
}

enum Flow {
    Next,
    Exit,
}

fn reg_offset(index: usize) -> i32 {
    let field = match index {
        0 => offset_of!(Registers, b),
        1 => offset_of!(Registers, c),
        2 => offset_of!(Registers, d),
        3 => offset_of!(Registers, e),
        H => offset_of!(Registers, h),
        L => offset_of!(Registers, l),
        F => offset_of!(Registers, f),
        _ => offset_of!(Registers, a),
    };
    (offset_of!(CPU, regs) + field) as i32
}

fn mmu_offset(field: usize) -> i32 {
    (offset_of!(CPU, mmu) + field) as i32
}

fn sp_offset() -> i32 {
    (offset_of!(CPU, regs) + offset_of!(Registers, sp)) as i32
}

fn pc_offset() -> i32 {
    (offset_of!(CPU, regs) + offset_of!(Registers, pc)) as i32
}

struct Emitter<'a> {
    b: FunctionBuilder<'a>,
    ptr: Type,
    cpu: Value,
    regs: [Variable; 8],
    sp: Variable,
    ticks: Variable,
    budget: Value,
    exit: Block,
    exit_request: Option<Value>,
    cycle: i64,
    oam_bug: bool,
    read_sig: SigRef,
    write_sig: SigRef,
    interpret_sig: SigRef,
}

impl<'a> Emitter<'a> {
    fn new(mut b: FunctionBuilder<'a>, ptr: Type, sigs: [Signature; 3]) -> Self {
        let entry = b.create_block();
        b.append_block_params_for_function_params(entry);
        b.switch_to_block(entry);
        let cpu = b.block_params(entry)[0];
        let [interpret, read, write] = sigs;
        let interpret_sig = b.import_signature(interpret);
        let read_sig = b.import_signature(read);
        let write_sig = b.import_signature(write);
        let regs = [(); 8].map(|_| b.declare_var(types::I32));
        let sp = b.declare_var(types::I32);
        let ticks = b.declare_var(types::I32);
        let budget = b.ins().load(
            types::I32,
            MemFlagsData::trusted(),
            cpu,
            offset_of!(CPU, jit_budget) as i32,
        );
        let zero = b.ins().iconst(types::I32, 0);
        b.def_var(ticks, zero);
        let exit = b.create_block();
        b.append_block_param(exit, types::I32);
        b.append_block_param(exit, types::I32);
        let mut emitter = Emitter {
            b,
            ptr,
            cpu,
            regs,
            sp,
            ticks,
            budget,
            exit,
            exit_request: None,
            cycle: 0,
            oam_bug: false,
            read_sig,
            write_sig,
            interpret_sig,
        };
        emitter.reload();
        emitter
    }

    fn emit_block(mut self, mmu: &MMU, start: u16, end: u32, config: TargetFrontendConfig) -> u32 {
        let instructions = decode(mmu, start, end);
        let count = instructions.len() as u32;

        let (body, skip) = (self.b.create_block(), self.b.create_block());
        let enough = self.b.ins().icmp_imm_u(
            IntCC::UnsignedGreaterThanOrEqual,
            self.budget,
            MAX_INSTRUCTION_TICKS * count as i64,
        );
        self.b.ins().brif(enough, body, &[], skip, &[]);
        self.b.switch_to_block(skip);
        let nothing = self.b.ins().iconst(types::I64, 0);
        self.b.ins().return_(&[nothing]);
        self.b.switch_to_block(body);

        for (index, inst) in (1..).zip(&instructions) {
            self.cycle = inst.next.wrapping_sub(inst.pc) as i64;
            let flow = self.instruction(inst.op, inst.pc, inst.next, index, inst.imm8, inst.imm16);
            if let Flow::Exit = flow {
                break;
            }
            if index == count {
                self.exit_to(inst.next, index);
            } else {
                self.end_instruction(inst.next, index);
            }
        }
        self.emit_exit();
        self.b.seal_all_blocks();
        self.b.finalize(config);
        instructions.last().unwrap().next as u32
    }

    fn emit_exit(&mut self) {
        self.b.switch_to_block(self.exit);
        let pc = self.b.block_params(self.exit)[0];
        let instructions = self.b.block_params(self.exit)[1];
        self.flush();
        self.b
            .ins()
            .istore16(MemFlagsData::trusted(), pc, self.cpu, pc_offset());
        let ticks = self.b.use_var(self.ticks);
        let ticks = self.b.ins().uextend(types::I64, ticks);
        let instructions = self.b.ins().uextend(types::I64, instructions);
        let instructions = self.b.ins().ishl_imm_u(instructions, 32);
        let result = self.b.ins().bor(ticks, instructions);
        self.b.ins().return_(&[result]);
    }

    fn end_instruction(&mut self, next: u16, n: u32) {
        let Some(request) = self.exit_request.take() else {
            return;
        };
        let leave = self.b.ins().icmp_imm_u(IntCC::NotEqual, request, 0);
        let pc = self.imm(next as i64);
        let n = self.imm(n as i64);
        let next_block = self.b.create_block();
        self.b
            .ins()
            .brif(leave, self.exit, &[pc.into(), n.into()], next_block, &[]);
        self.b.switch_to_block(next_block);
    }

    fn exit_to(&mut self, pc: u16, n: u32) {
        let pc = self.imm(pc as i64);
        self.exit_with(pc, n);
    }

    fn exit_with(&mut self, pc: Value, n: u32) {
        let n = self.imm(n as i64);
        self.b.ins().jump(self.exit, &[pc.into(), n.into()]);
    }

    fn imm(&mut self, value: i64) -> Value {
        self.b.ins().iconst(types::I32, value)
    }

    fn get(&mut self, reg: usize) -> Value {
        self.b.use_var(self.regs[reg])
    }

    fn set(&mut self, reg: usize, value: Value) {
        self.b.def_var(self.regs[reg], value);
    }

    fn get_pair(&mut self, pair: u8, af: bool) -> Value {
        let (hi, lo) = match pair {
            0 => (0, 1),
            1 => (2, 3),
            2 => (H, L),
            _ if af => (A, F),
            _ => return self.b.use_var(self.sp),
        };
        let hi = self.get(hi);
        let hi = self.b.ins().ishl_imm_u(hi, 8);
        let lo = self.get(lo);
        self.b.ins().bor(hi, lo)
    }

    fn set_pair(&mut self, pair: u8, af: bool, value: Value) {
        let (hi, lo) = match pair {
            0 => (0, 1),
            1 => (2, 3),
            2 => (H, L),
            _ if af => (A, F),
            _ => return self.b.def_var(self.sp, value),
        };
        let high = self.b.ins().ushr_imm_u(value, 8);
        let lo_mask = if lo == F { 0xF0 } else { 0xFF };
        let low = self.b.ins().band_imm_u(value, lo_mask);
        self.set(hi, high);
        self.set(lo, low);
    }

    fn flush(&mut self) {
        for reg in [0, 1, 2, 3, H, L, F, A] {
            let value = self.get(reg);
            self.b
                .ins()
                .istore8(MemFlagsData::trusted(), value, self.cpu, reg_offset(reg));
        }
        let sp = self.b.use_var(self.sp);
        self.b
            .ins()
            .istore16(MemFlagsData::trusted(), sp, self.cpu, sp_offset());
    }

    fn reload(&mut self) {
        for reg in [0, 1, 2, 3, H, L, F, A] {
            let value = self.b.ins().uload8(
                types::I32,
                MemFlagsData::trusted(),
                self.cpu,
                reg_offset(reg),
            );
            self.set(reg, value);
        }
        let sp = self
            .b
            .ins()
            .uload16(types::I32, MemFlagsData::trusted(), self.cpu, sp_offset());
        self.b.def_var(self.sp, sp);
    }

    fn store_ticks(&mut self) {
        let ticks = self.b.use_var(self.ticks);
        let ticks = self.b.ins().iadd_imm_s(ticks, self.cycle);
        self.b.ins().store(
            MemFlagsData::trusted(),
            ticks,
            self.cpu,
            offset_of!(CPU, jit_ticks) as i32,
        );
    }

    fn tick(&mut self, ticks: i64) {
        let current = self.b.use_var(self.ticks);
        let ticks = self.b.ins().iadd_imm_s(current, ticks);
        self.b.def_var(self.ticks, ticks);
    }

    fn tick_value(&mut self, ticks: Value) {
        let current = self.b.use_var(self.ticks);
        let ticks = self.b.ins().iadd(current, ticks);
        self.b.def_var(self.ticks, ticks);
    }

    fn call(&mut self, sig: SigRef, callee: usize, args: &[Value]) -> Value {
        let callee = self.b.ins().iconst(self.ptr, callee as i64);
        let mut call_args = vec![self.cpu];
        call_args.extend_from_slice(args);
        let call = self.b.ins().call_indirect(sig, callee, &call_args);
        self.b.inst_results(call)[0]
    }

    fn byte_ptr(&mut self, index: Value) -> Value {
        let index = self.b.ins().uextend(self.ptr, index);
        self.b.ins().iadd(self.cpu, index)
    }

    /// The memory regions accessed without calling into the MMU: WRAM bank 0,
    /// the switchable WRAM bank at D000 and HRAM. Each entry is the condition
    /// for `addr` being inside, its index into the backing array and the
    /// array's offset in the CPU.
    fn ram_regions(&mut self, addr: Value) -> [(Value, Value, i32); 3] {
        [
            (0xC000, 0x1000, offset_of!(MMU, work_ram)),
            (0xD000, 0x1000, offset_of!(MMU, work_ram)),
            (0xFF80, 0x7F, offset_of!(MMU, high_ram)),
        ]
        .map(|(base, size, field)| {
            let mut index = self.b.ins().iadd_imm_s(addr, -base);
            let inside = self
                .b
                .ins()
                .icmp_imm_u(IntCC::UnsignedLessThan, index, size);
            if base == 0xD000 {
                let bank_offset = self.b.ins().load(
                    types::I32,
                    MemFlagsData::trusted(),
                    self.cpu,
                    mmu_offset(offset_of!(MMU, work_ram_bank_offset)),
                );
                index = self.b.ins().iadd_imm_s(index, 0x1000);
                index = self.b.ins().iadd(index, bank_offset);
            }
            (inside, index, mmu_offset(field))
        })
    }

    fn internal(&mut self) {
        self.cycle += 1;
    }

    /// An internal cycle with `addr` on the address bus, which corrupts OAM
    /// on the DMG when it points there.
    fn internal_at(&mut self, addr: Value) {
        if self.oam_bug {
            let page = self.b.ins().ushr_imm_u(addr, 8);
            let in_oam = self.b.ins().icmp_imm_u(IntCC::Equal, page, 0xFE);
            let (corrupt, done) = (self.b.create_block(), self.b.create_block());
            self.b.ins().brif(in_oam, corrupt, &[], done, &[]);
            self.b.switch_to_block(corrupt);
            self.store_ticks();
            self.call(self.read_sig, jit_oam_bug as *const () as usize, &[addr]);
            self.b.ins().jump(done, &[]);
            self.b.switch_to_block(done);
        }
        self.internal();
    }

    fn read(&mut self, addr: Value) -> Value {
        self.cycle += 1;
        let done = self.b.create_block();
        self.b.append_block_param(done, types::I32);
        for (inside, index, offset) in self.ram_regions(addr) {
            let (fast, next) = (self.b.create_block(), self.b.create_block());
            self.b.ins().brif(inside, fast, &[], next, &[]);
            self.b.switch_to_block(fast);
            let ptr = self.byte_ptr(index);
            let value = self
                .b
                .ins()
                .uload8(types::I32, MemFlagsData::trusted(), ptr, offset);
            self.b.ins().jump(done, &[value.into()]);
            self.b.switch_to_block(next);
        }
        self.store_ticks();
        let value = self.call(self.read_sig, jit_read as *const () as usize, &[addr]);
        self.b.ins().jump(done, &[value.into()]);
        self.b.switch_to_block(done);
        self.b.block_params(done)[0]
    }

    fn write(&mut self, addr: Value, value: Value) {
        self.cycle += 1;
        let done = self.b.create_block();
        self.b.append_block_param(done, types::I32);
        let slow = self.b.create_block();
        let addr_ptr = self.byte_ptr(addr);
        for (inside, index, offset) in self.ram_regions(addr) {
            let (check, store, next) = (
                self.b.create_block(),
                self.b.create_block(),
                self.b.create_block(),
            );
            self.b.ins().brif(inside, check, &[], next, &[]);
            self.b.switch_to_block(check);
            let code_refs = self.b.ins().uload8(
                types::I32,
                MemFlagsData::trusted(),
                addr_ptr,
                mmu_offset(offset_of!(MMU, code_refs)),
            );
            self.b.ins().brif(code_refs, slow, &[], store, &[]);
            self.b.switch_to_block(store);
            let ptr = self.byte_ptr(index);
            self.b
                .ins()
                .istore8(MemFlagsData::trusted(), value, ptr, offset);
            let no_exit = self.imm(0);
            self.b.ins().jump(done, &[no_exit.into()]);
            self.b.switch_to_block(next);
        }
        self.b.ins().jump(slow, &[]);
        self.b.switch_to_block(slow);
        self.store_ticks();
        let request = self.call(
            self.write_sig,
            jit_write as *const () as usize,
            &[addr, value],
        );
        self.b.ins().jump(done, &[request.into()]);
        self.b.switch_to_block(done);
        let request = self.b.block_params(done)[0];
        self.request_exit(request);
    }

    fn request_exit(&mut self, request: Value) {
        self.exit_request = Some(match self.exit_request {
            Some(previous) => self.b.ins().bor(previous, request),
            None => request,
        });
    }

    fn interpret(&mut self, pc: u16) {
        self.flush();
        let pc = self.imm(pc as i64);
        self.b
            .ins()
            .istore16(MemFlagsData::trusted(), pc, self.cpu, pc_offset());
        self.cycle = 0;
        self.store_ticks();
        let result = self.call(self.interpret_sig, jit_interpret as *const () as usize, &[]);
        self.reload();
        let ticks = self.b.ins().band_imm_u(result, 0xFFFF);
        self.tick_value(ticks);
        let request = self.b.ins().ushr_imm_u(result, 16);
        self.request_exit(request);
    }

    fn interpret_and_exit(&mut self, pc: u16, n: u32) -> Flow {
        self.interpret(pc);
        let pc = self
            .b
            .ins()
            .uload16(types::I32, MemFlagsData::trusted(), self.cpu, pc_offset());
        self.exit_with(pc, n);
        Flow::Exit
    }

    fn bool(&mut self, cc: IntCC, x: Value, y: Value) -> Value {
        let result = self.b.ins().icmp(cc, x, y);
        self.b.ins().uextend(types::I32, result)
    }

    fn bool_imm(&mut self, cc: IntCC, x: Value, y: i64) -> Value {
        let result = self.b.ins().icmp_imm_u(cc, x, y);
        self.b.ins().uextend(types::I32, result)
    }

    fn low_nibble(&mut self, value: Value) -> Value {
        self.b.ins().band_imm_u(value, 0x0F)
    }

    fn carry(&mut self) -> Value {
        let f = self.get(F);
        let c = self.b.ins().ushr_imm_u(f, 4);
        self.b.ins().band_imm_u(c, 1)
    }

    fn flags(&mut self, z: Flag, n: Flag, h: Flag, c: Flag) {
        let mut keep = 0x0F;
        let mut set = 0;
        let mut values = Vec::new();
        for (bit, flag) in [(7, z), (6, n), (5, h), (4, c)] {
            match flag {
                Flag::Keep => keep |= 1 << bit,
                Flag::Clear => {}
                Flag::Set => set |= 1 << bit,
                Flag::Value(value) => values.push((bit, value)),
            }
        }
        let f = self.get(F);
        let mut f = self.b.ins().band_imm_u(f, keep);
        if set != 0 {
            f = self.b.ins().bor_imm_u(f, set);
        }
        for (bit, value) in values {
            let shifted = self.b.ins().ishl_imm_u(value, bit);
            f = self.b.ins().bor(f, shifted);
        }
        self.set(F, f);
    }

    fn zero(&mut self, value: Value) -> Flag {
        Flag::Value(self.bool_imm(IntCC::Equal, value, 0))
    }

    fn condition(&mut self, cc: u8) -> Value {
        let mask = if cc < 2 { 0x80 } else { 0x10 };
        let f = self.get(F);
        let bit = self.b.ins().band_imm_u(f, mask);
        let cond = if cc % 2 == 0 {
            IntCC::Equal
        } else {
            IntCC::NotEqual
        };
        self.b.ins().icmp_imm_u(cond, bit, 0)
    }

    fn operand(&mut self, reg: u8) -> Value {
        if reg as usize == 6 {
            let hl = self.get_pair(2, false);
            self.read(hl)
        } else {
            self.get(reg as usize)
        }
    }

    fn push(&mut self, value: Value) {
        let sp = self.b.use_var(self.sp);
        self.internal_at(sp);
        let hi = self.b.ins().ushr_imm_u(value, 8);
        let lo = self.b.ins().band_imm_u(value, 0xFF);
        for byte in [hi, lo] {
            let sp = self.b.use_var(self.sp);
            let sp = self.b.ins().iadd_imm_s(sp, -1);
            let sp = self.b.ins().band_imm_u(sp, 0xFFFF);
            self.b.def_var(self.sp, sp);
            self.write(sp, byte);
        }
    }

    fn pop(&mut self) -> Value {
        let mut bytes = [None; 2];
        for byte in &mut bytes {
            let sp = self.b.use_var(self.sp);
            *byte = Some(self.read(sp));
            let sp = self.b.ins().iadd_imm_s(sp, 1);
            let sp = self.b.ins().band_imm_u(sp, 0xFFFF);
            self.b.def_var(self.sp, sp);
        }
        let hi = self.b.ins().ishl_imm_u(bytes[1].unwrap(), 8);
        self.b.ins().bor(hi, bytes[0].unwrap())
    }

    fn inc8(&mut self, value: Value) -> Value {
        let result = self.b.ins().iadd_imm_s(value, 1);
        let result = self.b.ins().band_imm_u(result, 0xFF);
        let z = self.zero(result);
        let nibble = self.low_nibble(value);
        let h = self.bool_imm(IntCC::Equal, nibble, 0x0F);
        self.flags(z, Flag::Clear, Flag::Value(h), Flag::Keep);
        result
    }

    fn dec8(&mut self, value: Value) -> Value {
        let result = self.b.ins().iadd_imm_s(value, -1);
        let result = self.b.ins().band_imm_u(result, 0xFF);
        let z = self.zero(result);
        let nibble = self.low_nibble(value);
        let h = self.bool_imm(IntCC::Equal, nibble, 0x00);
        self.flags(z, Flag::Set, Flag::Value(h), Flag::Keep);
        result
    }

    fn alu(&mut self, op: u8, value: Value) {
        let a = self.get(A);
        let result = match op {
            0 | 1 => {
                let carry = if op == 1 { self.carry() } else { self.imm(0) };
                let sum = self.b.ins().iadd(a, value);
                let sum = self.b.ins().iadd(sum, carry);
                let result = self.b.ins().band_imm_u(sum, 0xFF);
                let a_nibble = self.low_nibble(a);
                let v_nibble = self.low_nibble(value);
                let nibbles = self.b.ins().iadd(a_nibble, v_nibble);
                let nibbles = self.b.ins().iadd(nibbles, carry);
                let h = self.bool_imm(IntCC::UnsignedGreaterThan, nibbles, 0x0F);
                let c = self.bool_imm(IntCC::UnsignedGreaterThan, sum, 0xFF);
                let z = self.zero(result);
                self.flags(z, Flag::Clear, Flag::Value(h), Flag::Value(c));
                Some(result)
            }
            2 | 3 | 7 => {
                let carry = if op == 3 { self.carry() } else { self.imm(0) };
                let subtrahend = self.b.ins().iadd(value, carry);
                let difference = self.b.ins().isub(a, subtrahend);
                let result = self.b.ins().band_imm_u(difference, 0xFF);
                let a_nibble = self.low_nibble(a);
                let v_nibble = self.low_nibble(value);
                let v_nibble = self.b.ins().iadd(v_nibble, carry);
                let h = self.bool(IntCC::UnsignedLessThan, a_nibble, v_nibble);
                let c = self.bool(IntCC::UnsignedLessThan, a, subtrahend);
                let z = self.zero(result);
                self.flags(z, Flag::Set, Flag::Value(h), Flag::Value(c));
                (op != 7).then_some(result)
            }
            4 => {
                let result = self.b.ins().band(a, value);
                let z = self.zero(result);
                self.flags(z, Flag::Clear, Flag::Set, Flag::Clear);
                Some(result)
            }
            5 | 6 => {
                let result = if op == 5 {
                    self.b.ins().bxor(a, value)
                } else {
                    self.b.ins().bor(a, value)
                };
                let z = self.zero(result);
                self.flags(z, Flag::Clear, Flag::Clear, Flag::Clear);
                Some(result)
            }
            _ => unreachable!(),
        };
        if let Some(result) = result {
            self.set(A, result);
        }
    }

    fn cb(&mut self, op: u8) {
        let y = (op >> 3) & 7;
        let z = op & 7;
        let value = self.operand(z);
        let result = match op >> 6 {
            0 => Some(self.shift(y, value)),
            1 => {
                let bit = self.b.ins().band_imm_u(value, 1 << y);
                let zero = self.zero(bit);
                self.flags(zero, Flag::Clear, Flag::Set, Flag::Keep);
                None
            }
            2 => Some(self.b.ins().band_imm_u(value, !(1i64 << y) & 0xFF)),
            _ => Some(self.b.ins().bor_imm_u(value, 1 << y)),
        };
        if let Some(result) = result {
            if z == 6 {
                let hl = self.get_pair(2, false);
                self.write(hl, result);
            } else {
                self.set(z as usize, result);
            }
        }
        self.tick(match (z, op >> 6) {
            (6, 1) => 3,
            (6, _) => 4,
            _ => 2,
        });
    }

    fn shift(&mut self, kind: u8, value: Value) -> Value {
        let high = self.b.ins().ushr_imm_u(value, 7);
        let low = self.b.ins().band_imm_u(value, 1);
        let (carry, result) = match kind {
            0 | 2 | 4 => {
                let fill = match kind {
                    0 => high,
                    2 => self.carry(),
                    _ => self.imm(0),
                };
                let shifted = self.b.ins().ishl_imm_u(value, 1);
                let result = self.b.ins().bor(shifted, fill);
                (Flag::Value(high), self.b.ins().band_imm_u(result, 0xFF))
            }
            1 | 3 | 5 | 7 => {
                let fill = match kind {
                    1 => self.b.ins().ishl_imm_u(low, 7),
                    3 => {
                        let carry = self.carry();
                        self.b.ins().ishl_imm_u(carry, 7)
                    }
                    5 => self.b.ins().band_imm_u(value, 0x80),
                    _ => self.imm(0),
                };
                let shifted = self.b.ins().ushr_imm_u(value, 1);
                (Flag::Value(low), self.b.ins().bor(shifted, fill))
            }
            _ => {
                let upper = self.b.ins().ushr_imm_u(value, 4);
                let lower = self.b.ins().ishl_imm_u(value, 4);
                let swapped = self.b.ins().bor(upper, lower);
                (Flag::Clear, self.b.ins().band_imm_u(swapped, 0xFF))
            }
        };
        let zero = self.zero(result);
        self.flags(zero, Flag::Clear, Flag::Clear, carry);
        result
    }

    fn rotate_a(&mut self, op: u8) {
        let a = self.get(A);
        let (carry_out, shifted, carry_in_shift) = if op == 0x07 || op == 0x17 {
            (
                self.b.ins().ushr_imm_u(a, 7),
                self.b.ins().ishl_imm_u(a, 1),
                0,
            )
        } else {
            (
                self.b.ins().band_imm_u(a, 1),
                self.b.ins().ushr_imm_u(a, 1),
                7,
            )
        };
        let carry_in = if op == 0x07 || op == 0x0F {
            carry_out
        } else {
            self.carry()
        };
        let carry_in = self.b.ins().ishl_imm_u(carry_in, carry_in_shift);
        let result = self.b.ins().bor(shifted, carry_in);
        let result = self.b.ins().band_imm_u(result, 0xFF);
        self.set(A, result);
        self.flags(
            Flag::Clear,
            Flag::Clear,
            Flag::Clear,
            Flag::Value(carry_out),
        );
    }

    fn instruction(&mut self, op: u8, pc: u16, next: u16, n: u32, imm8: u8, imm16: u16) -> Flow {
        let y = (op >> 3) & 7;
        let z = op & 7;
        let pair = (op >> 4) & 3;
        match op {
            0x00 => self.tick(1),
            0x01 | 0x11 | 0x21 | 0x31 => {
                let value = self.imm(imm16 as i64);
                self.set_pair(pair, false, value);
                self.tick(3);
            }
            0x02 | 0x12 => {
                let addr = self.get_pair(pair, false);
                let a = self.get(A);
                self.write(addr, a);
                self.tick(2);
            }
            0x0A | 0x1A => {
                let addr = self.get_pair(pair, false);
                let value = self.read(addr);
                self.set(A, value);
                self.tick(2);
            }
            0x22 | 0x32 | 0x2A | 0x3A => {
                let hl = self.get_pair(2, false);
                if op & 0x08 == 0 {
                    let a = self.get(A);
                    self.write(hl, a);
                } else {
                    let value = self.read(hl);
                    self.set(A, value);
                }
                let delta = if op & 0x10 == 0 { 1 } else { -1 };
                let hl = self.b.ins().iadd_imm_s(hl, delta);
                let hl = self.b.ins().band_imm_u(hl, 0xFFFF);
                self.set_pair(2, false, hl);
                self.tick(2);
            }
            0x03 | 0x13 | 0x23 | 0x33 | 0x0B | 0x1B | 0x2B | 0x3B => {
                let value = self.get_pair(pair, false);
                self.internal_at(value);
                let delta = if op & 0x08 == 0 { 1 } else { -1 };
                let value = self.b.ins().iadd_imm_s(value, delta);
                let value = self.b.ins().band_imm_u(value, 0xFFFF);
                self.set_pair(pair, false, value);
                self.tick(2);
            }
            0x34 | 0x35 => {
                let hl = self.get_pair(2, false);
                let value = self.read(hl);
                let result = if op == 0x34 {
                    self.inc8(value)
                } else {
                    self.dec8(value)
                };
                self.write(hl, result);
                self.tick(3);
            }
            0x04 | 0x0C | 0x14 | 0x1C | 0x24 | 0x2C | 0x3C => {
                let value = self.get(y as usize);
                let result = self.inc8(value);
                self.set(y as usize, result);
                self.tick(1);
            }
            0x05 | 0x0D | 0x15 | 0x1D | 0x25 | 0x2D | 0x3D => {
                let value = self.get(y as usize);
                let result = self.dec8(value);
                self.set(y as usize, result);
                self.tick(1);
            }
            0x36 => {
                let hl = self.get_pair(2, false);
                let value = self.imm(imm8 as i64);
                self.write(hl, value);
                self.tick(3);
            }
            0x06 | 0x0E | 0x16 | 0x1E | 0x26 | 0x2E | 0x3E => {
                let value = self.imm(imm8 as i64);
                self.set(y as usize, value);
                self.tick(2);
            }
            0x07 | 0x0F | 0x17 | 0x1F => {
                self.rotate_a(op);
                self.tick(1);
            }
            0x09 | 0x19 | 0x29 | 0x39 => {
                let hl = self.get_pair(2, false);
                let value = self.get_pair(pair, false);
                let sum = self.b.ins().iadd(hl, value);
                let hl_low = self.b.ins().band_imm_u(hl, 0x0FFF);
                let value_low = self.b.ins().band_imm_u(value, 0x0FFF);
                let low = self.b.ins().iadd(hl_low, value_low);
                let h = self.bool_imm(IntCC::UnsignedGreaterThan, low, 0x0FFF);
                let c = self.bool_imm(IntCC::UnsignedGreaterThan, sum, 0xFFFF);
                self.flags(Flag::Keep, Flag::Clear, Flag::Value(h), Flag::Value(c));
                let sum = self.b.ins().band_imm_u(sum, 0xFFFF);
                self.set_pair(2, false, sum);
                self.tick(2);
            }
            0x2F => {
                let a = self.get(A);
                let a = self.b.ins().bxor_imm_u(a, 0xFF);
                self.set(A, a);
                self.flags(Flag::Keep, Flag::Set, Flag::Set, Flag::Keep);
                self.tick(1);
            }
            0x37 => {
                self.flags(Flag::Keep, Flag::Clear, Flag::Clear, Flag::Set);
                self.tick(1);
            }
            0x3F => {
                let carry = self.carry();
                let carry = self.b.ins().bxor_imm_u(carry, 1);
                self.flags(Flag::Keep, Flag::Clear, Flag::Clear, Flag::Value(carry));
                self.tick(1);
            }
            0x18 => {
                self.tick(3);
                self.exit_to(next.wrapping_add(imm8 as i8 as u16), n);
                return Flow::Exit;
            }
            0x20 | 0x28 | 0x30 | 0x38 => {
                let target = next.wrapping_add(imm8 as i8 as u16);
                return self.conditional_jump(y - 4, target, next, 3, 2, n);
            }
            0xC2 | 0xCA | 0xD2 | 0xDA => {
                return self.conditional_jump(y, imm16, next, 4, 3, n);
            }
            0xC3 => {
                self.tick(4);
                self.exit_to(imm16, n);
                return Flow::Exit;
            }
            0xE9 => {
                let hl = self.get_pair(2, false);
                self.tick(1);
                self.exit_with(hl, n);
                return Flow::Exit;
            }
            0x40..=0x7F if op != 0x76 => {
                let value = self.operand(z);
                if y == 6 {
                    let hl = self.get_pair(2, false);
                    self.write(hl, value);
                } else {
                    self.set(y as usize, value);
                }
                self.tick(if y == 6 || z == 6 { 2 } else { 1 });
            }
            0x80..=0xBF => {
                let value = self.operand(z);
                self.alu(y, value);
                self.tick(if z == 6 { 2 } else { 1 });
            }
            0xC6 | 0xCE | 0xD6 | 0xDE | 0xE6 | 0xEE | 0xF6 | 0xFE => {
                let value = self.imm(imm8 as i64);
                self.alu(y, value);
                self.tick(2);
            }
            0xC1 | 0xD1 | 0xE1 | 0xF1 => {
                let value = self.pop();
                self.set_pair(pair, true, value);
                self.tick(3);
            }
            0xC5 | 0xD5 | 0xE5 | 0xF5 => {
                let value = self.get_pair(pair, true);
                self.push(value);
                self.tick(4);
            }
            0xC9 => {
                let pc = self.pop();
                self.tick(4);
                self.exit_with(pc, n);
                return Flow::Exit;
            }
            0xC0 | 0xC8 | 0xD0 | 0xD8 => {
                self.internal();
                let cond = self.condition(y);
                let (taken, skipped) = (self.b.create_block(), self.b.create_block());
                self.b.ins().brif(cond, taken, &[], skipped, &[]);
                self.b.switch_to_block(taken);
                let pc = self.pop();
                self.tick(5);
                self.exit_with(pc, n);
                self.b.switch_to_block(skipped);
                self.tick(2);
                self.exit_to(next, n);
                return Flow::Exit;
            }
            0xCD => {
                let ret = self.imm(next as i64);
                self.push(ret);
                self.tick(6);
                self.exit_to(imm16, n);
                return Flow::Exit;
            }
            0xC4 | 0xCC | 0xD4 | 0xDC => {
                let cond = self.condition(y);
                let (taken, skipped) = (self.b.create_block(), self.b.create_block());
                self.b.ins().brif(cond, taken, &[], skipped, &[]);
                self.b.switch_to_block(taken);
                let ret = self.imm(next as i64);
                self.push(ret);
                self.tick(6);
                self.exit_to(imm16, n);
                self.b.switch_to_block(skipped);
                self.tick(3);
                self.exit_to(next, n);
                return Flow::Exit;
            }
            0xC7 | 0xCF | 0xD7 | 0xDF | 0xE7 | 0xEF | 0xF7 | 0xFF => {
                let ret = self.imm(next as i64);
                self.push(ret);
                self.tick(4);
                self.exit_to(y as u16 * 8, n);
                return Flow::Exit;
            }
            0xE0 | 0xE2 | 0xEA => {
                let addr = match op {
                    0xE0 => self.imm(0xFF00 + imm8 as i64),
                    0xE2 => {
                        let c = self.get(1);
                        self.b.ins().iadd_imm_s(c, 0xFF00)
                    }
                    _ => self.imm(imm16 as i64),
                };
                let a = self.get(A);
                self.write(addr, a);
                self.tick(match op {
                    0xE0 => 3,
                    0xE2 => 2,
                    _ => 4,
                });
            }
            0xF0 | 0xF2 | 0xFA => {
                let addr = match op {
                    0xF0 => self.imm(0xFF00 + imm8 as i64),
                    0xF2 => {
                        let c = self.get(1);
                        self.b.ins().iadd_imm_s(c, 0xFF00)
                    }
                    _ => self.imm(imm16 as i64),
                };
                let value = self.read(addr);
                self.set(A, value);
                self.tick(match op {
                    0xF0 => 3,
                    0xF2 => 2,
                    _ => 4,
                });
            }
            0xF9 => {
                let hl = self.get_pair(2, false);
                self.internal_at(hl);
                self.b.def_var(self.sp, hl);
                self.tick(2);
            }
            0x10 | 0x76 | 0xD9 | 0xFB | 0xD3 | 0xDB | 0xDD | 0xE3 | 0xE4 | 0xEB | 0xEC | 0xED
            | 0xF4 | 0xFC | 0xFD => return self.interpret_and_exit(pc, n),
            0xCB => self.cb(imm8),
            _ => self.interpret(pc),
        }
        Flow::Next
    }

    fn conditional_jump(
        &mut self,
        cc: u8,
        target: u16,
        next: u16,
        taken_ticks: i64,
        skipped_ticks: i64,
        n: u32,
    ) -> Flow {
        let cond = self.condition(cc);
        let target = self.imm(target as i64);
        let next = self.imm(next as i64);
        let pc = self.b.ins().select(cond, target, next);
        let taken_ticks = self.imm(taken_ticks);
        let skipped_ticks = self.imm(skipped_ticks);
        let ticks = self.b.ins().select(cond, taken_ticks, skipped_ticks);
        self.tick_value(ticks);
        self.exit_with(pc, n);
        Flow::Exit
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cartridge::Cartridge;
    use crate::mmu::Model;

    fn assert_lockstep(
        rom: &[u8],
        model: Model,
        hot_threshold: u16,
        steps: usize,
        stop_at: Option<u16>,
    ) {
        let rom = rom.to_vec();
        let mut interpreted = CPU::new(Cartridge::new(rom.clone(), None), model);
        let mut jitted = CPU::new(Cartridge::new(rom.clone(), None), model);
        let mut jit = Jit::new(&jitted);
        jit.hot_threshold = hot_threshold;
        let state = |cpu: &CPU| {
            (
                cpu.regs,
                cpu.halted,
                cpu.halt_bug,
                cpu.stopped,
                cpu.ime_toggle,
            )
        };
        for step in 0..steps {
            if stop_at == Some(jitted.regs.pc) {
                return;
            }
            let start = jitted.regs;
            let start_ppu = format!(
                "{} IE {:02X} IF {:02X} budget {}",
                jitted.mmu.ppu.describe(),
                jitted.mmu.read_byte(0xFFFF),
                jitted.mmu.read_byte(0xFF0F),
                jitted.mmu.m_cycles_until_event(true)
            );
            let result = jit.step(&mut jitted);
            let ticks: usize = (0..result.instructions).map(|_| interpreted.cycle()).sum();
            let memory_differs = interpreted.mmu.apu.take_samples()
                != jitted.mmu.apu.take_samples()
                || !interpreted.mmu.same_state(&jitted.mmu);
            if ticks != result.ticks || state(&interpreted) != state(&jitted) || memory_differs {
                panic!(
                    "diverged at step {step} after {} instructions from {start:?}\n\
                     ticks: interpreter {ticks}, jit {}, memory differs: {memory_differs}\n\
                     interpreter: {:?}\n\
                     jit:         {:?}\n\
                     hardware at start: {start_ppu}\n\
                     hardware now: {}",
                    result.instructions,
                    result.ticks,
                    state(&interpreted),
                    state(&jitted),
                    jitted.mmu.ppu.describe(),
                );
            }
        }
    }

    #[test]
    #[ignore = "needs roms/cpu_instrs.gb"]
    fn matches_interpreter_on_cpu_instrs() {
        let rom = std::fs::read("roms/cpu_instrs.gb").unwrap();
        assert_lockstep(&rom, Model::Dmg, 16, 20_000_000, None);
    }

    struct Rng(u64);

    impl Rng {
        fn next(&mut self) -> u64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            self.0
        }

        fn below(&mut self, n: usize) -> usize {
            self.next() as usize % n
        }

        fn byte(&mut self) -> u8 {
            self.next() as u8
        }

        fn pick<T: Copy>(&mut self, items: &[T]) -> T {
            items[self.below(items.len())]
        }

        fn wram(&mut self) -> [u8; 2] {
            let addr = 0xC000 + self.below(0x0800) as u16;
            addr.to_le_bytes()
        }
    }

    const SUBROUTINE: u16 = 0x2000;
    const RAM_ROUTINE: u16 = 0xC800;
    const CODE_START: u16 = 0x150;

    fn register_op(rng: &mut Rng, moves_sp: bool) -> Vec<u8> {
        let reg = || [0u8, 1, 2, 3, 4, 5, 7];
        match rng.below(if moves_sp { 12 } else { 10 }) {
            0 => vec![0x40 | rng.pick(&reg()) << 3 | rng.pick(&reg())],
            1 => vec![0x80 | (rng.below(8) as u8) << 3 | rng.pick(&reg())],
            2 => vec![0xC6 | (rng.below(8) as u8) << 3, rng.byte()],
            3 => vec![0x04 | rng.pick(&reg()) << 3 | rng.below(2) as u8],
            4 => vec![rng.pick(&[0x03, 0x13, 0x23, 0x0B, 0x1B, 0x2B, 0x09, 0x19, 0x29])],
            5 => vec![rng.pick(&[0x07, 0x0F, 0x17, 0x1F, 0x27, 0x2F, 0x37, 0x3F, 0x00])],
            6 => vec![0xCB, rng.byte() & 0xF8 | rng.pick(&reg())],
            7 => vec![0x06 | rng.pick(&reg()) << 3, rng.byte()],
            8 => vec![0xF5, rng.pick(&[0xC1, 0xD1, 0xE1, 0xF1])],
            9 => vec![rng.pick(&[0xC5, 0xD5, 0xE5]), rng.pick(&[0xC1, 0xD1, 0xE1])],
            10 => vec![0xF8, rng.byte(), 0xF9, 0x31, 0x00, 0xD0],
            _ => vec![0xE8, rng.byte(), 0x31, 0x00, 0xD0],
        }
    }

    fn memory_op(rng: &mut Rng) -> Vec<u8> {
        let [lo, hi] = rng.wram();
        let mut code = vec![0x21, lo, hi];
        match rng.below(15) {
            10 => {
                let [lo, hi] = (0x8000 + rng.below(0x2000) as u16).to_le_bytes();
                code = vec![0x21, lo, hi, rng.pick(&[0x77, 0x7E, 0x34])];
            }
            11 => {
                let op = rng.pick(&[0x77, 0x7E, 0x23, 0x2B, 0x22, 0x32, 0x2A, 0x3A]);
                code = vec![0x21, rng.byte(), 0xFE, op];
            }
            12 => code = vec![0xF0, rng.pick(&[0x41, 0x44, 0x26, 0x12, 0x30, 0x00])],
            13 => {
                let port = rng.pick(&[
                    0x10, 0x11, 0x12, 0x13, 0x14, 0x16, 0x17, 0x19, 0x1A, 0x1C, 0x1E, 0x21, 0x22,
                    0x23, 0x24, 0x25, 0x26, 0x30, 0x40, 0x41, 0x42, 0x43, 0x45, 0x47, 0x4A, 0x4B,
                ]);
                let value = match port {
                    0x40 => rng.pick(&[0x91, 0x93, 0xB3, 0xE3, 0x11]),
                    0x45 => rng.below(154) as u8,
                    _ => rng.byte(),
                };
                code = vec![0x3E, value, 0xE0, port];
            }
            14 => code = vec![0x3E, 0xC0 + rng.below(8) as u8, 0xE0, 0x46],
            0 => code.push(0x46 | (rng.pick(&[0u8, 1, 2, 3, 7])) << 3),
            1 => code.push(0x70 | rng.pick(&[0u8, 1, 2, 3, 4, 5, 7])),
            2 => code.push(0x86 | (rng.below(8) as u8) << 3),
            3 => code.push(rng.pick(&[0x34, 0x35])),
            4 => code.extend([0x36, rng.byte()]),
            5 => code.push(rng.pick(&[0x22, 0x32, 0x2A, 0x3A])),
            6 => code.extend([0xCB, rng.byte() & 0xF8 | 6]),
            7 => {
                let pair = rng.pick(&[0x00, 0x10]);
                code.extend([0x01 | pair, lo, hi, rng.pick(&[0x02, 0x0A]) | pair]);
            }
            8 => code.extend([rng.pick(&[0xEA, 0xFA]), lo, hi]),
            _ => {
                let port = rng.pick(&[
                    0x80, 0x9F, 0xC0, 0xFE, 0x04, 0x05, 0x06, 0x07, 0x01, 0x02, 0x0F,
                ]);
                code.extend([rng.pick(&[0xE0, 0xF0]), port]);
            }
        }
        code
    }

    fn control_op(rng: &mut Rng, at: u16) -> Vec<u8> {
        let [sub_lo, sub_hi] = SUBROUTINE.to_le_bytes();
        let [ram_lo, ram_hi] = RAM_ROUTINE.to_le_bytes();
        match rng.below(9) {
            0 => vec![rng.pick(&[0x20, 0x28, 0x30, 0x38, 0x18]), 0x01, 0x00],
            1 => {
                let [lo, hi] = (at + 4).to_le_bytes();
                vec![rng.pick(&[0xC2, 0xCA, 0xD2, 0xDA, 0xC3]), lo, hi, 0x00]
            }
            2 => vec![rng.pick(&[0xC4, 0xCC, 0xD4, 0xDC, 0xCD]), sub_lo, sub_hi],
            3 => vec![0xFF],
            4 => vec![rng.pick(&[0xF3, 0xFB, 0x76]), 0x00],
            5 => vec![0xCD, ram_lo, ram_hi],
            6 => vec![
                0x3E,
                rng.pick(&[0x04, 0x0C, 0x15, 0x3C, 0xAF]),
                0xEA,
                ram_lo,
                ram_hi,
            ],
            7 => {
                let [lo, hi] = (at + 5).to_le_bytes();
                vec![0x21, lo, hi, 0xE9, 0x00]
            }
            _ => vec![0xCD, ram_lo, ram_hi, 0xCD, ram_lo, ram_hi],
        }
    }

    const BANKED_ROUTINE: u16 = 0xD800;

    fn cgb_op(rng: &mut Rng) -> Vec<u8> {
        let [lo, hi] = BANKED_ROUTINE.to_le_bytes();
        match rng.below(8) {
            0 => vec![0x3E, rng.below(8) as u8, 0xE0, 0x70],
            1 => vec![0x3E, rng.below(2) as u8, 0xE0, 0x4F],
            2 => {
                let (index_port, data_port) = rng.pick(&[(0x68, 0x69), (0x6A, 0x6B)]);
                vec![
                    0x3E,
                    rng.byte() | 0x80,
                    0xE0,
                    index_port,
                    0x3E,
                    rng.byte(),
                    0xE0,
                    data_port,
                ]
            }
            3 => vec![0xCD, lo, hi],
            4 => vec![0x3E, rng.pick(&[0x04, 0x14, 0x1D, 0xAF]), 0xEA, lo, hi],
            5 => {
                let mode = rng.pick(&[0x00, 0x80]);
                vec![
                    0x3E,
                    0xC0 + rng.below(8) as u8,
                    0xE0,
                    0x51,
                    0x3E,
                    rng.byte() & 0xF0,
                    0xE0,
                    0x52,
                    0x3E,
                    rng.below(0x20) as u8,
                    0xE0,
                    0x53,
                    0x3E,
                    rng.byte() & 0xF0,
                    0xE0,
                    0x54,
                    0x3E,
                    mode | rng.below(4) as u8,
                    0xE0,
                    0x55,
                ]
            }
            6 => vec![0x3E, 0x01, 0xE0, 0x4D, 0x10, 0x00],
            _ => vec![0x3E, rng.below(2) as u8, 0xE0, 0x6C],
        }
    }

    fn random_rom(seed: u64, cgb: bool) -> (Vec<u8>, u16) {
        let mut rng = Rng(seed.wrapping_mul(0x9E37_79B9_7F4A_7C15) | 1);
        let mut rom = vec![0u8; 0x8000];
        rom[0x38] = 0xC9;
        rom[0x40..0x42].copy_from_slice(&[0x04, 0xD9]);
        rom[0x48..0x4A].copy_from_slice(&[0x0C, 0xD9]);
        rom[0x50..0x52].copy_from_slice(&[0x1C, 0xD9]);

        let mut sub = Vec::new();
        for _ in 0..rng.below(6) {
            sub.extend(register_op(&mut rng, false));
        }
        sub.extend([0xD8, 0x3C, 0xC9]);
        rom[SUBROUTINE as usize..SUBROUTINE as usize + sub.len()].copy_from_slice(&sub);

        let [ram_lo, ram_hi] = RAM_ROUTINE.to_le_bytes();
        let mut code = vec![
            0x31,
            0x00,
            0xD0,
            0x3E,
            0x05,
            0xE0,
            0x07,
            0x3E,
            0x07,
            0xE0,
            0xFF,
            0x3E,
            rng.byte() & 0x78,
            0xE0,
            0x41,
        ];
        for (i, byte) in [0x04, 0x0C, 0xC9].into_iter().enumerate() {
            code.extend([0x3E, byte, 0xEA, ram_lo + i as u8, ram_hi]);
        }
        if cgb {
            rom[0x143] = 0x80;
            let [lo, hi] = BANKED_ROUTINE.to_le_bytes();
            for bank in 1..=7u8 {
                code.extend([0x3E, bank, 0xE0, 0x70]);
                let first = [0x04, 0x0C, 0x14, 0x1C, 0x05, 0x0D, 0x15][bank as usize - 1];
                for (i, byte) in [first, 0x2C, 0xC9].into_iter().enumerate() {
                    code.extend([0x3E, byte, 0xEA, lo + i as u8, hi]);
                }
            }
        }
        code.push(0xFB);
        while code.len() < 0x1E00 - CODE_START as usize {
            let at = CODE_START + code.len() as u16;
            code.extend(match rng.below(if cgb { 4 } else { 3 }) {
                0 => register_op(&mut rng, true),
                1 => memory_op(&mut rng),
                2 => control_op(&mut rng, at),
                _ => cgb_op(&mut rng),
            });
        }
        let end = CODE_START + code.len() as u16;
        code.extend([0x18, 0xFE]);
        rom[0x100..0x104].copy_from_slice(&[0x00, 0xC3, 0x50, 0x01]);
        rom[CODE_START as usize..CODE_START as usize + code.len()].copy_from_slice(&code);
        (rom, end)
    }

    #[test]
    fn matches_interpreter_on_random_programs() {
        let seeds = std::env::var("YARGB_FUZZ_SEEDS")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(40);
        for seed in 0..seeds {
            let model = if seed % 2 == 0 {
                Model::Dmg
            } else {
                Model::Cgb
            };
            let (rom, end) = random_rom(seed, model == Model::Cgb);
            for hot_threshold in [0, 3] {
                assert_lockstep(&rom, model, hot_threshold, 200_000, Some(end));
            }
        }
    }
}
