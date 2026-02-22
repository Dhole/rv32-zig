const std = @import("std");
const Allocator = std.mem.Allocator;

const root = @import("root.zig");
const Inst = root.Inst;
const Cpu = root.Cpu;
const Memory = root.Memory;

const disasm = @import("disasm.zig");
const InstFmt = disasm.InstFmt;

fn test_cpu_init(allocator: Allocator) !Cpu {
    const mem = try Memory.init(allocator, 0x80000000, 0x8000, 0x80002000, 4096);
    return Cpu.init(mem);
}

test "cpu" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();
    var cpu = try test_cpu_init(allocator);
    defer cpu.deinit();
    const inst = Inst.init(0b0000000_00011_00010_000_00001_0110011);
    cpu.exec(inst);
}

fn test_load_bin(cpu: *Cpu, bin_path: []u8) !void {
    var rom_file = try std.fs.cwd().openFile(bin_path, .{ .mode = .read_only });
    defer rom_file.close();

    const rom_file_size = (try rom_file.stat()).size;
    var reader = rom_file.reader(cpu.mem.rom);
    try reader.interface.fill(rom_file_size);
}

fn test_exec_bin(allocator: Allocator, bin_path: []u8) !Cpu {
    var cpu = try test_cpu_init(allocator);
    try test_load_bin(&cpu, bin_path);
    cpu.pc = 0x80000000;
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

    var cpu = try test_cpu_init(allocator);
    defer cpu.deinit();

    const op_str = "add";
    std.debug.print("> {s}\n", .{op_str});
    const bin_path = try std.fmt.allocPrint(allocator, "riscv-tests/isa/rv32ui-p-{s}.bin", .{op_str});

    _ = try test_exec_bin(allocator, bin_path);
}
