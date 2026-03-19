const std = @import("std");
const eql = std.mem.eql;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

const root = @import("root.zig");
const Inst = root.Inst;
const Cpu = root.BasicCpu;
const Memory = root.BasicMemory;

const disasm = @import("disasm.zig");
const InstFmt = disasm.InstFmt;

const debug = @import("debug.zig");
const MemoryFmt = debug.MemoryFmt;

const argsParser = @import("args");

const elfy = @import("elfy");

const Linenoise = @import("linenoize").Linenoise;

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
    const mem = try Memory.init(allocator, 0x80000000, 0x1000, 0x80001000, 0x8000);
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

    const rom_addr = cpu.mem.inner.rom_offset;
    const rom_size = cpu.mem.inner.rom.len;
    const ram_addr = cpu.mem.inner.ram_offset;
    const ram_size = cpu.mem.inner.ram.len;

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
            @memset(cpu.mem.inner.rom[offset .. offset + vsize], 0);
            @memcpy(cpu.mem.inner.rom[offset .. offset + data.len], data);
        } else if (flags.write() & ((ram_addr <= vaddr) & (vaddr + vsize <= ram_addr + ram_size))) {
            const data = try elf.getProgramData(segment);
            const offset = vaddr - ram_addr;
            @memset(cpu.mem.inner.ram[offset .. offset + vsize], 0);
            @memcpy(cpu.mem.inner.ram[offset .. offset + data.len], data);
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
    help,
    unknown,
    nop,
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

    var writer_buf: [128]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&writer_buf);
    var w = &stdout.interface;
    defer w.flush() catch unreachable;

    var last_cmd: Cmd = .nop;
    // loop:
    while (true) {
        try w.flush();
        var buf_input: ?[]const u8 = undefined;
        var free_input = false;
        if (init_cmd_it.next()) |cmd_str| {
            try w.print("$> {s}\n", .{cmd_str});
            buf_input = cmd_str;
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
                _ = a;
                // cpu.dbg.trace_inst = a.v;
            },
            .trace_io => |a| {
                _ = a;
                // cpu.dbg.trace_io = a.v;
            },
            .step => |a| {
                _ = a;
                // for (0..a.n) |_| {
                //     _ = cpu.step();
                // }
            },
            .@"continue" => {
                // const exit = run(&cpu);
                // if (exit) {
                //     return;
                // }
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
            .@"break" => |a| {
                _ = a;
                // for (cpu.dbg.breaks.items) |addr| {
                //     if (addr == a.addr) {
                //         try w.print("ERR: Duplicate breakpoint\n", .{});
                //         continue :loop;
                //     }
                // }
                // try cpu.dbg.breaks.append(a.addr);
            },
            .delete_break => |a| {
                _ = a;
                // for (cpu.dbg.breaks.items, 0..) |addr, i| {
                //     if (addr == a.addr) {
                //         _ = cpu.dbg.breaks.orderedRemove(i);
                //         continue :loop;
                //     }
                // }
                // try w.print("ERR: Breakpoint not found\n", .{});
            },
            .info_breaks => {
                // for (cpu.dbg.breaks.items, 0..) |addr, i| {
                //     try w.print("break {d} at {x:0>8}\n", .{ i, addr });
                // }
            },
            .regs => {
                // try cpu.format_regs(w);
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
    const arg = it.next() orelse return error.MissingArgument;
    return std.fmt.parseInt(u32, arg, 16) catch error.InvalidArgument;
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
