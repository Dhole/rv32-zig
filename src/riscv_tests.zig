const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const expectEqual = testing.expectEqual;

const elfy = @import("elfy");

const root = @import("root.zig");
const Inst = root.Inst;
const Cpu = root.Cpu;
const Memory = root.Memory;

const disasm = @import("disasm.zig");
const InstFmt = disasm.InstFmt;

const debug = @import("debug.zig");
const MemoryFmt = debug.MemoryFmt;

fn test_cpu_init(allocator: Allocator) !Cpu {
    const mem = try Memory.init(allocator, 0x80000000, 0x1000, 0x80001000, 0x8000);
    return Cpu.init(mem);
}

test "cpu" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();
    var cpu = try test_cpu_init(allocator);
    defer cpu.deinit();
    const inst = Inst.init(0b0000000_00011_00010_000_00001_0110011);
    _ = cpu.exec(inst);
}

fn test_load_bin(cpu: *Cpu, bin_path: []const u8) !void {
    var rom_file = try std.fs.cwd().openFile(bin_path, .{ .mode = .read_only });
    defer rom_file.close();

    const rom_file_size = (try rom_file.stat()).size;
    var reader = rom_file.reader(cpu.mem.rom);
    try reader.interface.fill(rom_file_size);
}

fn test_exec_bin(allocator: Allocator, bin_path: []const u8) !Cpu {
    var cpu = try test_cpu_init(allocator);
    try test_load_bin(&cpu, bin_path);
    cpu.pc = 0x80000000;
    var i: usize = 0;
    while (i < 0x1000) : (i += 1) {
        const word = cpu.mem.read(u32, cpu.pc);
        const inst = Inst.init(word);
        // std.debug.print("{x:0>8}:  {x:0>8}  ", .{ cpu.pc, word });
        // std.debug.print("{f}\n", .{InstFmt{ .addr = cpu.pc, .inst = inst }});
        const opt_trap = cpu.exec(inst);
        if (opt_trap) |trap| {
            if (trap == .ecall) {
                return cpu;
            } else {
                return error.unexpected_trap;
            }
        }
    }
    return error.too_many_steps;
}

test "rv32ui" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();

    const ops_str = [_][]const u8{
        "add",
        "addi",
        "and",
        "andi",
        "auipc",
        "beq",
        "bge",
        "bgeu",
        "blt",
        "bltu",
        "bne",
        // "fence_i", // FIXME: Requires different ELF sections
        "jal",
        "jalr",
        "lb",
        "lbu",
        // "ld_st", // FIXME
        "lh",
        "lhu",
        "lui",
        "lw",
        // "ma_data", // FIXME
        "or",
        "ori",
        // "sb", // FIXME
        // "sh", // FIXME
        "simple",
        "sll",
        "slli",
        "slt",
        "slti",
        "sltiu",
        "sltu",
        "sra",
        "srai",
        "srl",
        "srli",
        // "st_ld", // FIXME
        "sub",
        // "sw", // FIXME
        "xor",
        "xori",
    };
    for (ops_str) |op_str| {
        std.debug.print("> {s}\n", .{op_str});
        const bin_path = try std.fmt.allocPrint(allocator, "riscv-tests/isa/rv32ui-p-{s}.bin", .{op_str});

        const cpu = try test_exec_bin(allocator, bin_path);
        try expectEqual(0, cpu.regs[10]);
    }
}

test "rv32um" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();

    const ops_str = [_][]const u8{
        "div",
        "divu",
        "mul",
        "mulh",
        // "mulhsu", // FIXME
        // "mulhu", // FIXME
        "rem",
        "remu",
    };
    for (ops_str) |op_str| {
        std.debug.print("> {s}\n", .{op_str});
        const bin_path = try std.fmt.allocPrint(allocator, "riscv-tests/isa/rv32um-p-{s}.bin", .{op_str});

        const cpu = try test_exec_bin(allocator, bin_path);
        try expectEqual(0, cpu.regs[10]);
    }
}

fn test_exec_elf(allocator: Allocator, elf_path: []const u8) !Cpu {
    var cpu = try test_cpu_init(allocator);
    try test_load_elf(allocator, &cpu, elf_path);
    // std.debug.print("RAM:\n{f}", .{MemoryFmt{ .mem = &cpu.mem, .offset = 0x80001000, .size = 0x100 }});

    var i: usize = 0;
    while (i < 0x1000) : (i += 1) {
        const word = cpu.mem.read(u32, cpu.pc);
        const inst = Inst.init(word);
        // std.debug.print("{x:0>8}:  {x:0>8}  ", .{ cpu.pc, word });
        // std.debug.print("{f}\n", .{InstFmt{ .addr = cpu.pc, .inst = inst }});
        const opt_trap = cpu.exec(inst);
        if (opt_trap) |trap| {
            if (trap == .ecall) {
                return cpu;
            } else {
                return error.unexpected_trap;
            }
        }
    }
    return error.too_many_steps;
}

test "rv32_single" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();

    const elf_path: []const u8 = "riscv-tests/isa/rv32ui-p-st_ld";

    const cpu = try test_exec_elf(allocator, elf_path);
    try expectEqual(0, cpu.regs[10]);
}

fn test_load_elf(allocator: Allocator, cpu: *Cpu, elf_path: []const u8) !void {
    var elf = try elfy.Elf.init(elf_path, .ReadOnly, allocator);
    defer elf.deinit();

    cpu.pc = @intCast(elf.getHeader().getEntryPoint());

    const rom_addr = cpu.mem.rom_offset;
    const rom_size = cpu.mem.rom.len;
    const ram_addr = cpu.mem.ram_offset;
    const ram_size = cpu.mem.ram.len;

    var segments = try elf.getIterator(elfy.ElfProgram);
    while (try segments.next()) |segment| {
        if (segment.getType() != .PT_LOAD) {
            continue;
        }
        const vaddr = segment.getVirtualAddress();
        const vsize = segment.getMemorySize();
        const size = segment.getFileSize();
        try expectEqual(size, vsize);
        const flags = segment.getFlags();
        if (!flags.write() & ((rom_addr <= vaddr) & (vaddr + vsize <= rom_addr + rom_size))) {
            const data = try elf.getProgramData(segment);
            const offset = vaddr - rom_addr;
            @memcpy(cpu.mem.rom[offset .. offset + data.len], data);
        } else if (flags.write() & ((ram_addr <= vaddr) & (vaddr + vsize <= ram_addr + ram_size))) {
            const data = try elf.getProgramData(segment);
            const offset = vaddr - ram_addr;
            @memcpy(cpu.mem.ram[offset .. offset + data.len], data);
        } else {
            std.debug.panic("Unexpected segment config", .{});
        }
    }
}

test "elfy" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();
    const elf_path: []const u8 = "riscv-tests/isa/rv32ui-p-sw";
    var binary = try elfy.Elf.init(elf_path, .ReadOnly, allocator);
    defer binary.deinit();

    var segments = try binary.getIterator(elfy.ElfProgram);
    while (try segments.next()) |segment| {
        std.debug.print("@{x:0>8}:{x}", .{ segment.getOffset(), segment.getFileSize() });
        std.debug.print(" -> ", .{});
        std.debug.print("@{x:0>8}:{x}", .{ segment.getVirtualAddress(), segment.getMemorySize() });
        std.debug.print(" ({s})", .{@tagName(segment.getType())});
        const flags = segment.getFlags();
        std.debug.print(" [{s}{s}{s}]", .{
            if (flags.read()) "R" else "-",
            if (flags.write()) "W" else "-",
            if (flags.execute()) "X" else "-",
        });
        std.debug.print("\n", .{});
    }

    std.debug.print("\n", .{});

    var sections = try binary.getIterator(elfy.ElfSection);
    while (try sections.next()) |section| {
        const name = try binary.getSectionName(section);
        const section_type = section.getType();
        std.debug.print("Name: {s}, Type: {s}\n", .{ name, @tagName(section_type) });
    }
}
