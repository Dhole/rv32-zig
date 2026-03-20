const std = @import("std");
const Writer = std.Io.Writer;

const root = @import("root.zig");
// const Inst = root.Inst;
const MemoryInterface = root.MemoryInterface;
const Cpu = root.Cpu;
const Config = root.Config;

const disasm = @import("disasm.zig");

pub fn MemoryFmt(comptime M: type) type {
    return struct {
        mem: *const MemoryInterface(M),
        offset: u32,
        n: u32,

        const Self = @This();
        pub fn format(self: Self, w: *Writer) !void {
            const start = std.math.divFloor(u32, self.offset, 16) catch unreachable;
            const end = std.math.divCeil(u32, self.offset + self.n, 16) catch unreachable;
            for (start..end - 1) |x| {
                try w.print("{x:0>8}  ", .{x * 16});
                for (0..16 - 1) |y| {
                    const p: u32 = @intCast(x * 16 + y);
                    const byte = if ((self.offset <= p) & (p < self.offset + self.n))
                        self.mem._read(u8, p)
                    else
                        null;
                    if (byte) |b| {
                        try w.print("{x:0>2}", .{b});
                    } else {
                        try w.print("  ", .{});
                    }
                    if (p % 8 == 0) {
                        try w.print("  ", .{});
                    } else {
                        try w.print("  ", .{});
                    }
                }
                try w.print("\n", .{});
            }
        }
    };
}

pub fn CpuFmt(comptime cfg: Config) type {
    return struct {
        cpu: *Cpu(cfg),

        const Self = @This();
        pub fn format(self: Self, w: *Writer) !void {
            const cpu = self.cpu;
            for (0..8) |row| {
                for (0..5) |column| {
                    if (column == 0) {
                        switch (row) {
                            0 => try w.print("pc: {x:0>8}", .{cpu.pc}),
                            1 => try w.print("priv_mode: {s}", .{@tagName(cpu.priv_mode)}),
                            else => try w.print("            ", .{}),
                        }
                    } else {
                        const i: usize = @intCast((column - 1) * 8 + row);
                        const reg = if (i == 0) "zr" else disasm.reg[i];
                        try w.print("   {d:0>2} {s: >3}: {x:0>8}", .{ i, reg, cpu.regs[i] });
                    }
                }
                try w.print("\n", .{});
            }
        }
    };
}
