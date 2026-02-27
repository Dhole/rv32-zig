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

fn test_cpu_init(allocator: Allocator, config: []const u8) !Cpu {
    const mem = if (std.mem.eql(u8, config, "a"))
        try Memory.init(allocator, 0x80000000, 0x1000, 0x80001000, 0x8000)
    else if (std.mem.eql(u8, config, "b"))
        try Memory.init(allocator, 0x80000000, 0x2000, 0x80002000, 0x8000)
    else
        @panic("Invalid config");
    return Cpu.init(mem);
}

test "cpu" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();
    var cpu = try test_cpu_init(allocator, "a");
    defer cpu.deinit();
    const inst = Inst.init(0b0000000_00011_00010_000_00001_0110011);
    _ = cpu.exec(inst);
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
        const flags = segment.getFlags();
        if (!flags.write() & ((rom_addr <= vaddr) & (vaddr + vsize <= rom_addr + rom_size))) {
            const data = try elf.getProgramData(segment);
            const offset = vaddr - rom_addr;
            @memset(cpu.mem.rom[offset .. offset + vsize], 0);
            @memcpy(cpu.mem.rom[offset .. offset + data.len], data);
        } else if (flags.write() & ((ram_addr <= vaddr) & (vaddr + vsize <= ram_addr + ram_size))) {
            const data = try elf.getProgramData(segment);
            const offset = vaddr - ram_addr;
            @memset(cpu.mem.ram[offset .. offset + vsize], 0);
            @memcpy(cpu.mem.ram[offset .. offset + data.len], data);
        } else {
            std.debug.panic("Unexpected segment config", .{});
        }
    }
}

fn test_exec_elf(allocator: Allocator, elf_path: []const u8) !Cpu {
    const mem_config = if (std.mem.containsAtLeast(u8, elf_path, 1, "ld_st"))
        "b"
    else
        "a";
    var cpu = try test_cpu_init(allocator, mem_config);
    try test_load_elf(allocator, &cpu, elf_path);
    // std.debug.print("RAM:\n{f}", .{MemoryFmt{ .mem = &cpu.mem, .offset = 0x80001000, .size = 0x100 }});

    var i: usize = 0;
    while (i < 0x1000) : (i += 1) {
        const word = cpu.mem.read(u32, cpu.pc);
        const inst = Inst.init(word);
        std.debug.print("{x:0>8}:  {x:0>8}  ", .{ cpu.pc, word });
        std.debug.print("{f}\n", .{InstFmt{ .addr = cpu.pc, .inst = inst }});
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
        "fence_i",
        "jal",
        "jalr",
        "lb",
        "lbu",
        "ld_st",
        "lh",
        "lhu",
        "lui",
        "lw",
        "ma_data",
        "or",
        "ori",
        "sb",
        "sh",
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
        "st_ld",
        "sub",
        "sw",
        "xor",
        "xori",
    };
    for (ops_str) |op_str| {
        std.debug.print("> {s}\n", .{op_str});
        const elf_path = try std.fmt.allocPrint(allocator, "riscv-tests/isa/rv32ui-p-{s}", .{op_str});

        const cpu = try test_exec_elf(allocator, elf_path);
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
        "mulhsu",
        "mulhu",
        "rem",
        "remu",
    };
    for (ops_str) |op_str| {
        std.debug.print("> {s}\n", .{op_str});
        const elf_path = try std.fmt.allocPrint(allocator, "riscv-tests/isa/rv32um-p-{s}", .{op_str});

        const cpu = try test_exec_elf(allocator, elf_path);
        try expectEqual(0, cpu.regs[10]);
    }
}

test "rv32ua" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();

    const ops_str = [_][]const u8{
        "amoadd_w",
        "amoand_w",
        "amomaxu_w",
        "amomax_w",
        "amominu_w",
        "amomin_w",
        "amoor_w",
        "amoswap_w",
        "amoxor_w",
        "lrsc",
    };
    for (ops_str) |op_str| {
        std.debug.print("> {s}\n", .{op_str});
        const elf_path = try std.fmt.allocPrint(allocator, "riscv-tests/isa/rv32ua-p-{s}", .{op_str});

        const cpu = try test_exec_elf(allocator, elf_path);
        try expectEqual(0, cpu.regs[10]);
    }
}

test "rv32_single" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();

    const elf_path: []const u8 = "riscv-tests/isa/rv32ua-p-amoadd_w";

    const cpu = try test_exec_elf(allocator, elf_path);
    try expectEqual(0, cpu.regs[10]);
}

test "elfy" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();
    const elf_path: []const u8 = "riscv-tests/isa/rv32ui-p-ld_st";
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
