const std = @import("std");
const eql = std.mem.eql;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

const root = @import("root.zig");
const Inst = root.Inst;
const Config = root.Config;

const MemoryReadError = root.MemoryReadError;
const MemoryWriteError = root.MemoryWriteError;
const buf_read = root.buf_read;
const buf_write = root.buf_write;

const disasm = @import("disasm.zig");
const InstFmt = disasm.InstFmt;

const debug = @import("debug.zig");
const MemoryFmt = debug.MemoryFmt;
const CpuFmt = debug.CpuFmt;

const argsParser = @import("args");

const elfy = @import("elfy");

const Linenoise = @import("linenoize").Linenoise;

pub fn FlatMemory(comptime N: usize) type {
    return struct {
        allocator: Allocator,
        offset: u32,
        mem: []u8,
        reservation_addr: u32,
        reservation_valid: bool,
        // The last fault address seen
        fault_addr: u32,

        const Self = @This();

        pub fn init(allocator: Allocator, offset: u32) !Self {
            const mem = try allocator.alloc(u8, N);
            return Self{
                .allocator = allocator,
                .offset = offset,
                .mem = mem,
                .reservation_addr = 0,
                .reservation_valid = false,
                .fault_addr = 0,
            };
        }

        pub fn _read(self: *const Self, comptime T: type, addr: u32) ?T {
            if (self.offset <= addr and addr < self.offset + self.mem.len) {
                return buf_read(T, self.mem, addr - self.offset);
            }
            return null;
        }

        pub fn read(self: *Self, comptime T: type, addr: u32) MemoryReadError!T {
            return self._read(T, addr) orelse blk: {
                std.debug.print("invalid read at {x:08}\n", .{addr});
                self.fault_addr = addr;
                break :blk error.ReadInvalidAddr;
            };
        }

        pub fn write(self: *Self, comptime T: type, addr: u32, v: T) MemoryWriteError!void {
            if ((addr & ~@as(u32, 0b11)) == self.reservation_addr) {
                self.reservation_valid = false;
            }
            if (self.offset <= addr and addr < self.offset + self.mem.len) {
                return buf_write(T, self.mem, addr - self.offset, v);
            }
            std.debug.print("invalid write at {x:08}\n", .{addr});
            self.fault_addr = addr;
            return error.WriteInvalidAddr;
        }

        pub fn reserve(self: *Self, addr: u32) void {
            self.reservation_addr = addr;
            self.reservation_valid = true;
        }
        pub fn validate_reservation(self: *Self, addr: u32) bool {
            const valid = self.reservation_addr == addr and self.reservation_valid;
            self.reservation_valid = false;
            return valid;
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.mem);
        }
    };
}

const Memory = FlatMemory(32 * 1024 * 1024);
const Cpu = root.Cpu(Config{ .mem_type = Memory });

const Options = struct {
    cmd: ?[]const u8 = null,
    help: bool = false,

    pub const shorthands = .{
        .c = "cmd",
    };

    pub const meta = .{
        .option_docs = .{
            .cmd = "<CMD> Initial list of commands, separated by `;`",
            .help = "Print help",
        },
    };
};
const Verb = union(enum) {
    run_elf: struct {
        file: ?[]const u8 = null,

        pub const shorthands = .{
            .f = "file",
        };

        pub const meta = .{
            .option_docs = .{
                .file = "<BIN> ELF file path",
            },
        };
    },
};

fn printHelp(options: anytype) !void {
    var writer_buf: [128]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&writer_buf);
    defer stdout.interface.flush() catch unreachable;
    try argsParser.printHelpWithVerb(Options, Verb, options.executable_name orelse "bin_debug", &stdout.interface);
    return;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const options = argsParser.parseWithVerbForCurrentProcess(Options, Verb, allocator, .print) catch unreachable;
    defer options.deinit();
    const glob_opts = options.options;

    if (glob_opts.help) {
        try printHelp(options);
        return;
    }
    if (options.verb) |verb| {
        switch (verb) {
            .run_elf => |opts| {
                const file = opts.file orelse {
                    std.log.err("Missing argument 'file'", .{});
                    return;
                };
                return try run_elf(allocator, glob_opts, file);
            },
        }
    }
    try printHelp(options);
    return;
}

fn run_elf(allocator: Allocator, glob_opts: Options, elf_path: []const u8) !void {
    const mem = try Memory.init(allocator, 0x80000000);
    var cpu = Cpu.init(mem);
    defer cpu.mem.deinit();
    defer cpu.deinit();
    try load_elf(allocator, &cpu, elf_path);
    try run(allocator, glob_opts, &cpu);
}

fn load_elf(allocator: Allocator, cpu: *Cpu, elf_path: []const u8) !void {
    var elf = try elfy.Elf.init(elf_path, .ReadOnly, allocator);
    defer elf.deinit();

    cpu.pc = @intCast(elf.getHeader().getEntryPoint());

    const mem_addr = cpu.mem.inner.offset;
    const mem_size = cpu.mem.inner.mem.len;

    var segments = try elf.getIterator(elfy.ElfProgram);
    while (try segments.next()) |segment| {
        if (segment.getType() != .PT_LOAD) {
            continue;
        }
        const vaddr = segment.getVirtualAddress();
        const vsize = segment.getMemorySize();
        const flags = segment.getFlags();
        if (!flags.write() & ((mem_addr <= vaddr) & (vaddr + vsize <= mem_addr + mem_size))) {
            // READ
            const data = try elf.getProgramData(segment);
            const offset = vaddr - mem_addr;
            @memset(cpu.mem.inner.mem[offset .. offset + vsize], 0);
            @memcpy(cpu.mem.inner.mem[offset .. offset + data.len], data);
        } else if (flags.write() & ((mem_addr <= vaddr) & (vaddr + vsize <= mem_addr + mem_size))) {
            // WRITE
            const data = try elf.getProgramData(segment);
            const offset = vaddr - mem_addr;
            @memset(cpu.mem.inner.mem[offset .. offset + vsize], 0);
            @memcpy(cpu.mem.inner.mem[offset .. offset + data.len], data);
        } else {
            std.debug.panic("Unexpected segment config", .{});
        }
    }
}

const Cmd = union(enum) {
    step: struct { n: usize },
    trace_inst: struct { v: bool },
    trace_io: struct { v: bool },
    @"continue",
    @"break": struct { addr: u32 },
    delete_break: struct { addr: u32 },
    info_breaks,
    disasm: struct { n: u32 },
    disasm_at: struct { offset: u32, n: u32 },
    dump: struct { offset: u32, n: u32 },
    regs,
    csr: struct { addr: u12 },
    help,
    unknown,
    nop,
};

const AddressWatcher = struct {
    addrs: [32]u32,
    len: usize,
    mask: u32,

    const Self = @This();
    fn init() Self {
        return .{
            .addrs = .{0} ** 32,
            .len = 0,
            .mask = 0,
        };
    }
    fn add(self: *Self, addr: u32) !void {
        if (self.matches(addr)) {
            return error.AddressWatcherDuplicate;
        }
        if (self.len == self.addrs.len) {
            return error.AddressWatcherFull;
        }
        self.addrs[self.len] = addr;
        self.len += 1;
        self.update_mask();
    }
    fn del(self: *Self, addr: u32) !void {
        for (0..self.len) |i| {
            if (addr == self.addrs[i]) {
                @memmove(self.addrs[i .. self.len - 1], self.addrs[i + 1 .. self.len]);
                self.len -= 1;
                self.update_mask();
                return;
            }
        }
        return error.AddressWatcherAddrNotFound;
    }
    fn matches(self: *Self, addr: u32) bool {
        if ((addr & self.mask) != addr) {
            return false;
        }
        for (0..self.len) |i| {
            if (addr == self.addrs[i]) {
                return true;
            }
        }
        return false;
    }
    fn update_mask(self: *Self) void {
        var mask: u32 = 0;
        for (0..self.len) |i| {
            mask |= self.addrs[i];
        }
        self.mask = mask;
    }
};

const State = struct {
    trace_inst: bool,
    breakpoints: AddressWatcher,
    pause: bool,

    fn init() State {
        return .{
            .trace_inst = false,
            .breakpoints = AddressWatcher.init(),
            .pause = false,
        };
    }
};

fn run(allocator: Allocator, glob_opts: Options, cpu: *Cpu) !void {
    // const print = true;
    // var i: usize = 0;
    // while (i < 0x8000) : (i += 1) {
    //     if (print) {
    //         std.debug.print("{x:0>8}:  ", .{cpu.pc});
    //         if (cpu.mem.read(u32, cpu.pc)) |word| {
    //             const inst = Inst.init(word);
    //             std.debug.print("{x:0>8}  ", .{word});
    //             std.debug.print("{f}\n", .{InstFmt{ .addr = cpu.pc, .inst = inst }});
    //         } else |_| {
    //             std.debug.print("***\n", .{});
    //         }
    //     }
    //     cpu.step();
    //     const to_host_value = cpu.mem.read(u32, cpu.mem.inner.ram_offset) catch unreachable;
    //     if (to_host_value != 0) {
    //         if (to_host_value == 1) {
    //             return;
    //         } else {
    //             std.debug.print("fail code: {d}\n", .{to_host_value});
    //             return error.Fail;
    //         }
    //     }
    // }

    var init_cmd_it = std.mem.tokenizeSequence(u8, glob_opts.cmd orelse "", ";");

    var ln = Linenoise.init(allocator);
    defer ln.deinit();

    var writer_buf: [4096]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&writer_buf);
    var w = &stdout.interface;
    defer w.flush() catch unreachable;

    var state = State.init();
    var last_cmd: Cmd = .nop;
    // loop:
    while (true) {
        try w.flush();
        var buf_input: ?[]const u8 = undefined;
        var free_input = false;
        if (init_cmd_it.next()) |cmd_str| {
            const cmd_str_clean = std.mem.trimStart(u8, cmd_str, " \n");
            try w.print("$> {s}\n", .{cmd_str_clean});
            buf_input = cmd_str_clean;
        } else {
            buf_input = try ln.linenoise("> ");
            free_input = true;
        }
        const input = buf_input orelse break;
        defer {
            if (free_input) {
                allocator.free(input);
            }
        }
        try ln.history.add(input);

        const cmd = parse_input(input) catch |err| {
            switch (err) {
                error.InvalidArgument => try w.print("ERR: Invalid argument\n", .{}),
                error.MissingArgument => try w.print("ERR: Missing argument\n", .{}),
            }
            continue;
        } orelse last_cmd;

        switch (cmd) {
            .trace_inst => |a| {
                state.trace_inst = a.v;
            },
            .trace_io => |a| {
                _ = a;
                // cpu.dbg.trace_io = a.v;
            },
            .step => |a| {
                for (0..a.n) |_| {
                    if (state.trace_inst) {
                        try disasm_block(cpu, w, cpu.pc, 1);
                        try w.flush();
                    }
                    if (!state.pause and state.breakpoints.matches(cpu.pc)) {
                        try w.print("hit breakpoint at {x:0>8}", .{cpu.pc});
                        state.pause = true;
                        break;
                    } else {
                        state.pause = false;
                    }
                    cpu.step();
                }
            },
            .@"continue" => {
                while (true) {
                    if (state.trace_inst) {
                        try disasm_block(cpu, w, cpu.pc, 1);
                        try w.flush();
                    }
                    if (!state.pause and state.breakpoints.matches(cpu.pc)) {
                        try w.print("hit breakpoint at {x:0>8}\n", .{cpu.pc});
                        state.pause = true;
                        break;
                    } else {
                        state.pause = false;
                    }
                    cpu.step();
                }
            },
            .disasm => |a| {
                try disasm_block(cpu, w, cpu.pc, a.n);
            },
            .disasm_at => |a| {
                try disasm_block(cpu, w, a.offset, a.n);
            },
            .dump => |a| {
                try dump(cpu, w, a.offset, a.n);
            },
            .@"break" => |a| state.breakpoints.add(a.addr) catch |err| {
                switch (err) {
                    error.AddressWatcherDuplicate => try w.print("ERR: Duplicate breakpoint\n", .{}),
                    error.AddressWatcherFull => try w.print("ERR: Breakpoint list full\n", .{}),
                }
            },
            .delete_break => |a| state.breakpoints.del(a.addr) catch |err| {
                switch (err) {
                    error.AddressWatcherAddrNotFound => try w.print("ERR: Breakpoint not found\n", .{}),
                }
            },
            .info_breaks => {
                const breakpoints = &state.breakpoints;
                for (0..breakpoints.len) |i| {
                    try w.print("break {d} at {x:0>8}\n", .{ i, breakpoints.addrs[i] });
                }
            },
            .regs => {
                try w.print("{f}\n", .{CpuFmt(Cpu.cfg){ .cpu = cpu }});
            },
            .csr => |a| {
                var name = disasm.csr[a.addr];
                if (name.len == 0) {
                    name = "?";
                }
                try w.print("{x:0>3} ({s}): {x:0>8}\n", .{ a.addr, name, cpu.csr.regs[a.addr] });
            },
            .help => try print_help(w),
            .unknown => {
                try w.print("ERR: Unknown command\n", .{});
            },
            .nop => {},
        }
        last_cmd = cmd;
    }
}

fn print_help(w: *Writer) !void {
    try w.print(
        \\Commands:
        \\ h : Print help
        \\ help : Print help
        \\ s (n) : Step `n` or 1 instruction
        \\ c : Continue runnig until the next breakpoint
        \\ r : Print CPU Registers
        \\ trace_inst {{t,f}} : Enable/disable tracing all executed instructions
        \\ trace_io {{t,f}} : Enable/disable tracing all IO accesses
        \\ b [addr] : Add breakpoint at `addr`
        \\ db [addr] : Delete breakpoint at `addr`
        \\ ib : (Info) List all breakpoints
        \\ l (n) : List disassembly of `n` or 10 instructions at PC
        \\ la [addr] (n) : List disassembly of `n` or 10 instructions at `addr`
        \\ d [addr] (n) : Dump `n` or 256 bytes of memory at `addr`
        \\
    , .{});
}

fn disasm_block(cpu: *const Cpu, w: *Writer, offset: u32, n: u32) !void {
    for (0..n) |i| {
        const addr: u32 = @intCast(offset + i * 4);
        try w.print("{x:0>8}:  ", .{addr});
        if (cpu.mem._read(u32, addr)) |word| {
            const inst = Inst.init(word);
            try w.print("{x:0>8}  ", .{word});
            try w.print("{f}\n", .{InstFmt{ .addr = cpu.pc, .inst = inst }});
        } else {
            try w.print("***\n", .{});
        }
    }
}

fn dump(cpu: *const Cpu, w: *Writer, offset: u32, n: u32) !void {
    try w.print("{f}", .{MemoryFmt(Memory){ .mem = &cpu.mem, .offset = offset, .n = n }});
}

// it: *std.mem.TokenIterator
fn parse_bool(it: anytype) !bool {
    const arg = it.next() orelse return error.MissingArgument;
    if (eql(u8, arg, "t")) {
        return true;
    } else if (eql(u8, arg, "f")) {
        return false;
    } else {
        return error.InvalidArgument;
    }
}

// it: *std.mem.TokenIterator
fn parse_word(it: anytype) !u32 {
    return parse_hex(u32, it);
}

fn parse_hex(comptime T: type, it: anytype) !T {
    const arg = it.next() orelse return error.MissingArgument;
    return std.fmt.parseInt(T, arg, 16) catch error.InvalidArgument;
}

// it: *std.mem.TokenIterator
fn parse_int_or(comptime T: type, it: anytype, default: T) !T {
    const arg = it.next() orelse return default;
    return std.fmt.parseInt(T, arg, 0) catch error.InvalidArgument;
}

fn parse_input(input: []const u8) !?Cmd {
    var it = std.mem.tokenizeSequence(u8, input, " ");
    const cmd = it.next() orelse {
        return null;
    };
    if (eql(u8, cmd, "h") or eql(u8, cmd, "help")) {
        return .help;
    } else if (eql(u8, cmd, "r")) {
        return .regs;
    } else if (eql(u8, cmd, "csr")) {
        const addr = try parse_hex(u12, &it);
        return .{ .csr = .{ .addr = addr } };
    } else if (eql(u8, cmd, "c")) {
        return .@"continue";
    } else if (eql(u8, cmd, "ib")) {
        return .info_breaks;
    } else if (eql(u8, cmd, "b")) {
        const addr = try parse_word(&it);
        return .{ .@"break" = .{ .addr = addr } };
    } else if (eql(u8, cmd, "db")) {
        const addr = try parse_word(&it);
        return .{ .delete_break = .{ .addr = addr } };
    } else if (eql(u8, cmd, "trace_inst")) {
        const value = try parse_bool(&it);
        return .{ .trace_inst = .{ .v = value } };
    } else if (eql(u8, cmd, "trace_io")) {
        const value = try parse_bool(&it);
        return .{ .trace_io = .{ .v = value } };
    } else if (eql(u8, cmd, "s")) {
        const n = try parse_int_or(usize, &it, 1);
        return .{ .step = .{ .n = n } };
    } else if (eql(u8, cmd, "l")) {
        const n = try parse_int_or(u32, &it, 10);
        return .{ .disasm = .{ .n = n } };
    } else if (eql(u8, cmd, "la")) {
        const offset = try parse_word(&it);
        const n = try parse_int_or(u32, &it, 10);
        return .{ .disasm_at = .{ .offset = offset, .n = n } };
    } else if (eql(u8, cmd, "d")) {
        const offset = try parse_word(&it);
        const n = try parse_int_or(u32, &it, 16 * 8);
        return .{ .dump = .{ .offset = offset, .n = n } };
    } else {
        return .unknown;
    }
}
