const std = @import("std");
const Writer = std.Io.Writer;

const root = @import("root.zig");
// const Inst = root.Inst;
// const Cpu = root.Cpu;
const MemoryInterface = root.MemoryInterface;

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
