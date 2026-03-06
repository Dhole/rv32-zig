const std = @import("std");
const Allocator = std.mem.Allocator;
const debug = std.debug;
const log = std.log;

fn log_debug(src: std.builtin.SourceLocation, comptime fmt: []const u8, args: anytype) void {
    debug.print(fmt, args);
    debug.print("\n", .{});
    debug.print("  at {s}:{}\n", .{ src.file, src.line });
}

pub const Inst = packed struct {
    const Args = packed union {
        const RType = packed struct {
            rd: u5,
            funct3: u3,
            rs1: u5,
            rs2: u5,
            funct7: u7,
        };
        const RAType = packed struct {
            rd: u5,
            funct3: u3,
            rs1: u5,
            rs2: u5,
            rl: u1,
            aq: u1,
            funct5: u5,
        };
        const IType = packed struct {
            rd: u5,
            funct3: u3,
            rs1: u5,
            imm0_11: u12,
        };
        const BType = packed struct {
            imm11: u1,
            imm1_4: u4,
            funct3: u3,
            rs1: u5,
            rs2: u5,
            imm5_10: u6,
            imm12: u1,
        };
        const SType = packed struct {
            imm0_4: u5,
            funct3: u3,
            rs1: u5,
            rs2: u5,
            imm5_11: u7,
        };
        const UType = packed struct {
            rd: u5,
            imm12_31: u20,
        };
        const JType = packed struct {
            rd: u5,
            imm12_19: u8,
            imm11: u1,
            imm1_10: u10,
            imm20: u1,
        };

        r_type: RType,
        ra_type: RAType,
        i_type: IType,
        b_type: BType,
        s_type: SType,
        u_type: UType,
        j_type: JType,
    };

    _opcode: u7,
    args: Args,

    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 32);
    }

    const Self = @This();

    pub fn init(v: u32) Self {
        return @bitCast(v);
    }

    pub fn opcode(self: Self) u7 {
        return self._opcode;
    }

    pub fn rd(self: Self) u5 {
        return self.args.r_type.rd;
    }

    pub fn rs1(self: Self) u5 {
        return self.args.r_type.rs1;
    }

    pub fn rs2(self: Self) u5 {
        return self.args.r_type.rs2;
    }

    pub fn funct3(self: Self) u3 {
        return self.args.r_type.funct3;
    }

    pub fn funct5(self: Self) u5 {
        return self.args.ra_type.funct5;
    }

    pub fn funct7(self: Self) u7 {
        return self.args.r_type.funct7;
    }

    pub fn funct12(self: Self) u12 {
        return self.args.i_type.imm0_11;
    }

    pub fn csr(self: Self) u12 {
        return self.args.i_type.imm0_11;
    }

    pub fn shamt(self: Self) u5 {
        return self.args.r_type.rs2;
    }

    pub fn u_imm(self: Self) u32 {
        return @as(u32, self.args.u_type.imm12_31) << 12;
    }

    pub fn j_imm(self: Self) u21 {
        return @as(u21, self.args.j_type.imm1_10) << 1 |
            @as(u21, self.args.j_type.imm11) << 11 |
            @as(u21, self.args.j_type.imm12_19) << 12 |
            @as(u21, self.args.j_type.imm20) << 20;
    }

    pub fn signed_j_imm(self: Self) i32 {
        const imm: i21 = @bitCast(self.j_imm());
        return @as(i32, imm);
    }

    pub fn b_imm(self: Self) u13 {
        return @as(u13, self.args.b_type.imm1_4) << 1 |
            @as(u13, self.args.b_type.imm5_10) << 5 |
            @as(u13, self.args.b_type.imm11) << 11 |
            @as(u13, self.args.b_type.imm12) << 12;
    }

    pub fn signed_b_imm(self: Self) i32 {
        const imm: i13 = @bitCast(self.b_imm());
        return @as(i32, imm);
    }

    pub fn i_imm(self: Self) u12 {
        return @as(u12, self.args.i_type.imm0_11);
    }

    pub fn signed_i_imm(self: Self) i32 {
        const imm: i12 = @bitCast(self.i_imm());
        return @as(i32, imm);
    }

    pub fn s_imm(self: Self) u12 {
        return @as(u12, self.args.s_type.imm0_4) |
            @as(u12, self.args.s_type.imm5_11) << 5;
    }

    pub fn signed_s_imm(self: Self) i32 {
        const imm: i12 = @bitCast(self.s_imm());
        return @as(i32, imm);
    }

    pub fn csr_imm(self: Self) u32 {
        return @as(u32, self.args.r_type.rs1);
    }
};

pub fn buf_read(comptime T: type, buf: []const u8, addr: u32) T {
    return switch (T) {
        u8 => buf[addr],
        u16 => @as(u16, buf[addr]) + @as(u16, buf[addr + 1]) * 0x100,
        u32 => @as(u32, buf[addr]) + @as(u32, buf[addr + 1]) * 0x100 +
            @as(u32, buf[addr + 2]) * 0x10000 + @as(u32, buf[addr + 3]) * 0x1000000,
        else => @compileError("invalid T"),
    };
}

pub fn buf_write(comptime T: type, buf: []u8, addr: u32, v: T) void {
    switch (T) {
        u8 => {
            buf[addr] = @truncate(v);
        },
        u16 => {
            buf[addr + 0] = @truncate(v >> 0);
            buf[addr + 1] = @truncate(v >> 8);
        },
        u32 => {
            buf[addr + 0] = @truncate(v >> 0);
            buf[addr + 1] = @truncate(v >> 8);
            buf[addr + 2] = @truncate(v >> 16);
            buf[addr + 3] = @truncate(v >> 24);
        },
        else => @compileError("invalid T"),
    }
}

pub const BasicMemory = struct {
    allocator: Allocator,
    rom_offset: u32,
    rom: []u8,
    ram_offset: u32,
    ram: []u8,
    reservation_addr: u32,
    reservation_valid: bool,

    const Self = @This();

    pub fn init(allocator: Allocator, rom_offset: u32, rom_size: u32, ram_offset: u32, ram_size: u32) !Self {
        const rom = try allocator.alloc(u8, rom_size);
        const ram = try allocator.alloc(u8, ram_size);
        return Self{
            .allocator = allocator,
            .rom_offset = rom_offset,
            .rom = rom,
            .ram_offset = ram_offset,
            .ram = ram,
            .reservation_addr = 0,
            .reservation_valid = false,
        };
    }

    pub fn read(self: *Self, comptime T: type, addr: u32) MemoryReadError!T {
        if (self.rom_offset <= addr and addr < self.rom_offset + self.rom.len) {
            return buf_read(T, self.rom, addr - self.rom_offset);
        } else if (self.ram_offset <= addr and addr < self.ram_offset + self.ram.len) {
            return buf_read(T, self.ram, addr - self.ram_offset);
        }
        debug.print("invalid read at {x:08}\n", .{addr});
        return error.ReadInvalidAddr;
    }

    pub fn write(self: *Self, comptime T: type, addr: u32, v: T) MemoryWriteError!void {
        if ((addr & ~@as(u32, 0b11)) == self.reservation_addr) {
            self.reservation_valid = false;
        }
        if (self.ram_offset <= addr and addr < self.ram_offset + self.ram.len) {
            return buf_write(T, self.ram, addr - self.ram_offset, v);
        }
        debug.print("invalid write at {x:08}\n", .{addr});
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
        self.allocator.free(self.rom);
        self.allocator.free(self.ram);
    }
};

pub const PrivilegeMode = enum(u2) {
    U = 0b00,
    S = 0b01,
    M = 0b11,
    _,
};

pub const MStatus = packed struct {
    wpri0: u1,
    sie: u1,
    wpri2: u1,
    mie: u1,
    wpri4: u1,
    spie: u1,
    ube: u1,
    mpie: u1,
    spp: u1,
    vs: u2,
    mpp: u2,
    fs: u2,
    xs: u2,
    mprv: u1,
    sum: u1,
    mxr: u1,
    tvm: u1,
    tw: u1,
    tsr: u1,
    spelp: u1,
    sdt: u1,
    wpri25: u6,
    sd: u1,
};

pub const MTVec = packed struct {
    const Mode = enum(u2) {
        direct = 0,
        vectored = 1,
        _,
    };
    mode: Mode,
    base: u30,

    fn address(self: *const MTVec) u32 {
        return @as(u32, self.base) << 2;
    }
};

const ExceptionCode = enum(u31) {
    inst_addr_misaligned = 0,
    inst_access_fault = 1,
    illegal_inst = 2,
    load_addr_misaligned = 4,
    load_access_fault = 5,
    store_amo_addr_misaligned = 6,
    store_amo_access_fault = 7,
    env_call_from_u_mode = 8,
    env_call_from_m_mode = 11,
    _,
};

pub const MCause = packed struct {
    exception: ExceptionCode,
    interrupt: u1,
};

const Privilege = struct {
    mode: PrivilegeMode,
    read_only: bool,
};

const Csr = struct {
    regs: [4096]u32,

    // Machine Information Registers
    const MHARTID: u12 = 0xf14;
    // Machine Non-Maskable Interrupt Handling
    const MNSTATUS: u12 = 0x744;
    // Machine Memory Protection
    const PMPCFG0: u12 = 0x3a0;
    const PMPADDR0: u12 = 0x3b0;
    // Machine Trap Setup
    const MSTATUS: u12 = 0x300;
    const MEDELEG: u12 = 0x302;
    const MIDELEG: u12 = 0x303;
    const MIE: u12 = 0x304;
    const MTVEC: u12 = 0x305;
    // Machine Trap Handling
    const MEPC: u12 = 0x341;
    const MCAUSE: u12 = 0x342;

    // Supervisor Protection and Translation
    const SATP: u12 = 0x180;

    fn build_priv_table() [4096]Privilege {
        var table = [_]Privilege{.{ .mode = .M, .read_only = true }} ** 4096;
        table[MHARTID] = .{ .mode = .M, .read_only = true };

        table[MNSTATUS] = .{ .mode = .M, .read_only = false };

        table[MSTATUS] = .{ .mode = .M, .read_only = false };
        table[MEDELEG] = .{ .mode = .M, .read_only = false };
        table[MIDELEG] = .{ .mode = .M, .read_only = false };
        table[MIE] = .{ .mode = .M, .read_only = false };
        table[MTVEC] = .{ .mode = .M, .read_only = false };

        table[MEPC] = .{ .mode = .M, .read_only = false };
        table[MCAUSE] = .{ .mode = .M, .read_only = false };

        table[PMPCFG0] = .{ .mode = .M, .read_only = false };
        table[PMPADDR0] = .{ .mode = .M, .read_only = false };

        table[SATP] = .{ .mode = .S, .read_only = false };

        return table;
    }

    const priv_table = build_priv_table();

    const Self = @This();

    fn init() Self {
        var regs = [_]u32{0} ** 4096;
        regs[MHARTID] = 0;
        return .{
            .regs = regs,
        };
    }

    fn read(self: *Self, mode: PrivilegeMode, addr: u12) !u32 {
        if (@intFromEnum(mode) < @intFromEnum(priv_table[addr].mode)) {
            std.debug.print("CSR: Mode lower than required\n", .{});
            return error.CsrReadPriv;
        }
        switch (addr) {
            MHARTID, MSTATUS, MTVEC, MEPC, MCAUSE => {},
            // MHARTID, MNSTATUS, SATP, PMPCFG0, PMPADDR0, MSTATUS, MEDELEG, MIDELEG, MIE, MTVEC, MEPC, MCAUSE => {},
            else => std.debug.print("Unimplemented  read csr {x:0>3}\n", .{addr}),
        }
        return self.regs[addr];
    }

    fn write(self: *Self, mode: PrivilegeMode, addr: u12, v: u32) !void {
        if (@intFromEnum(mode) < @intFromEnum(priv_table[addr].mode)) {
            std.debug.print("CSR: Mode lower than required\n", .{});
            return error.CsrWritePriv;
        }
        if (priv_table[addr].read_only) {
            std.debug.print("CSR: is read-only: {x:0>3}\n", .{addr});
            return error.CsrWriteRo;
        }
        switch (addr) {
            MTVEC => {
                const mtvec: MTVec = @bitCast(v);
                if (mtvec.mode != .direct) {
                    std.debug.panic("Unimplemented mtvec.mode={s}", .{@tagName(mtvec.mode)});
                }
            },
            else => std.debug.print("Unimplemented write csr {x:0>3}\n", .{addr}),
        }
        self.regs[addr] = v;
    }
};

const CSR_MEPC: u32 = 0x341;
const CSR_MCAUSE: u32 = 0x342;
const CSR_MTVEC: u32 = 0x303;

pub const basic_config = Config{
    .mem_type = BasicMemory,
};

pub const BasicCpu = Cpu(basic_config);

fn is_misaligned(comptime T: type, addr: u32) bool {
    switch (T) {
        u8 => {},
        u16 => if ((addr & 0b1) != 0) {
            return true;
        },
        u32 => if ((addr & 0b11) != 0) {
            return true;
        },
        else => @compileError("invalid T"),
    }
    return false;
}

pub const MemoryReadError = error{
    ReadMisaligned,
    ReadInvalidAddr,
};
pub const MemoryWriteError = error{
    WriteMisaligned,
    WriteInvalidAddr,
};

pub fn MemoryInterface(comptime M: type) type {
    return struct {
        inner: M,
        const Self = @This();
        pub fn read(self: *Self, comptime T: type, addr: u32) MemoryReadError!T {
            if (is_misaligned(T, addr)) {
                return error.ReadMisaligned;
            }
            return self.inner.read(T, addr);
        }
        pub fn write(self: *Self, comptime T: type, addr: u32, v: T) MemoryWriteError!void {
            if (is_misaligned(T, addr)) {
                return error.WriteMisaligned;
            }
            return self.inner.write(T, addr, v);
        }
        pub fn reserve(self: *Self, addr: u32) MemoryReadError!void {
            if (is_misaligned(u32, addr)) {
                return error.ReadMisaligned;
            }
            self.inner.reserve(addr);
        }
        pub fn validate_reservation(self: *Self, addr: u32) MemoryWriteError!bool {
            if (is_misaligned(u32, addr)) {
                return error.WriteMisaligned;
            }
            return self.inner.validate_reservation(addr);
        }
        pub fn deinit(self: *Self) void {
            self.inner.deinit();
        }
    };
}

pub const Config = struct {
    mem_type: type,
};

pub fn Cpu(comptime cfg: Config) type {
    const Memory = MemoryInterface(cfg.mem_type);
    return struct {
        regs: [32]u32,
        pc: u32,
        priv_mode: PrivilegeMode,
        csr: Csr,
        mem: Memory,

        const Self = @This();

        pub fn init(mem: cfg.mem_type) Self {
            return .{
                .regs = .{0} ** 32,
                .pc = 0,
                .priv_mode = .M,
                .csr = Csr.init(),
                .mem = Memory{ .inner = mem },
            };
        }

        pub fn deinit(self: *Self) void {
            self.mem.deinit();
        }

        pub fn handle_exception(self: *Self, exception: ExceptionCode) void {
            self.csr.regs[Csr.MEPC] = self.pc;
            self.csr.regs[Csr.MCAUSE] = @as(u32, @intFromEnum(exception));
            // Set MTVAL to 0?
            var mstatus: MStatus = @bitCast(self.csr.regs[Csr.MSTATUS]);
            mstatus.mpp = @intFromEnum(self.priv_mode);
            mstatus.mpie = mstatus.mie;
            mstatus.mie = 0;
            self.csr.regs[Csr.MSTATUS] = @bitCast(mstatus);
            const mtvec: MTVec = @bitCast(self.csr.regs[Csr.MTVEC]);
            self.pc = mtvec.address();
            // TODO: set the correct priv mode
            self.priv_mode = .M;
        }

        pub fn step_debug(self: *Self) struct { ?Inst, ?ExceptionCode } {
            var opt_inst: ?Inst = null;
            const opt_exception: ?ExceptionCode = if (self.mem.read(u32, self.pc)) |word| blk: {
                const inst = Inst.init(word);
                opt_inst = inst;
                break :blk self.exec(inst);
            } else |err| switch (err) {
                error.ReadInvalidAddr => .inst_access_fault,
            };
            if (opt_exception) |exception| {
                self.handle_exception(exception);
            }
            return .{ opt_inst, opt_exception };
        }

        pub fn step(self: *Self) void {
            const opt_exception: ?ExceptionCode = if (self.mem.read(u32, self.pc)) |word| blk: {
                const inst = Inst.init(word);
                break :blk self.exec(inst);
            } else |err| switch (err) {
                error.ReadMisaligned => .inst_addr_misaligned,
                error.ReadInvalidAddr => .inst_access_fault,
            };
            if (opt_exception) |exception| {
                self.handle_exception(exception);
            }
        }

        pub fn exec(self: *Self, inst: Inst) ?ExceptionCode {
            self.exec_err(inst) catch |err| {
                return switch (err) {
                    error.ReadMisaligned => .load_addr_misaligned,
                    error.ReadInvalidAddr => .load_access_fault,
                    error.WriteMisaligned => .store_amo_addr_misaligned,
                    error.WriteInvalidAddr => .store_amo_access_fault,
                    error.UnkInst => .illegal_inst,
                    error.CsrReadPriv => .illegal_inst,
                    error.CsrWritePriv => .illegal_inst,
                    error.CsrWriteRo => .illegal_inst,
                    error.Ecall => switch (self.priv_mode) {
                        .M => .env_call_from_m_mode,
                        .U => .env_call_from_u_mode,
                        else => @panic("Unsupported"),
                    },
                };
            };
            return null;
        }

        fn exec_err(self: *Self, inst: Inst) !void {
            switch (inst.opcode()) {
                0b0110111 => { // LUI
                    self.regs[inst.rd()] = inst.u_imm();
                    self.pc +%= 4;
                },
                0b0010111 => { // AUIPC
                    self.regs[inst.rd()] = self.pc +% inst.u_imm();
                    self.pc +%= 4;
                },
                0b1101111 => { // JAL
                    self.regs[inst.rd()] = self.pc +% 4;
                    const imm: u32 = @bitCast(inst.signed_j_imm());
                    self.pc +%= imm;
                },
                0b1100111 => switch (inst.funct3()) {
                    0b000 => { // JALR
                        const next = self.pc +% 4;
                        const imm: u32 = @bitCast(inst.signed_i_imm());
                        self.pc = (self.regs[inst.rs1()] +% imm) & ~@as(u32, 0b1);
                        self.regs[inst.rd()] = next;
                    },
                    else => return error.UnkInst,
                },
                0b1100011 => switch (inst.funct3()) {
                    0b000 => { // BEQ
                        if (self.regs[inst.rs1()] == self.regs[inst.rs2()]) {
                            const imm: u32 = @bitCast(inst.signed_b_imm());
                            self.pc +%= imm;
                        } else {
                            self.pc +%= 4;
                        }
                    },
                    0b001 => { // BNE
                        if (self.regs[inst.rs1()] != self.regs[inst.rs2()]) {
                            const imm: u32 = @bitCast(inst.signed_b_imm());
                            self.pc +%= imm;
                        } else {
                            self.pc +%= 4;
                        }
                    },
                    0b100 => { // BLT
                        const rs1: i32 = @bitCast(self.regs[inst.rs1()]);
                        const rs2: i32 = @bitCast(self.regs[inst.rs2()]);
                        if (rs1 < rs2) {
                            const imm: u32 = @bitCast(inst.signed_b_imm());
                            self.pc +%= imm;
                        } else {
                            self.pc +%= 4;
                        }
                    },
                    0b101 => { // BGE
                        const rs1: i32 = @bitCast(self.regs[inst.rs1()]);
                        const rs2: i32 = @bitCast(self.regs[inst.rs2()]);
                        if (rs1 >= rs2) {
                            const imm: u32 = @bitCast(inst.signed_b_imm());
                            self.pc +%= imm;
                        } else {
                            self.pc +%= 4;
                        }
                    },
                    0b110 => { // BLTU
                        if (self.regs[inst.rs1()] < self.regs[inst.rs2()]) {
                            const imm: u32 = @bitCast(inst.signed_b_imm());
                            self.pc +%= imm;
                        } else {
                            self.pc +%= 4;
                        }
                    },
                    0b111 => { // BGEU
                        if (self.regs[inst.rs1()] >= self.regs[inst.rs2()]) {
                            const imm: u32 = @bitCast(inst.signed_b_imm());
                            self.pc +%= imm;
                        } else {
                            self.pc +%= 4;
                        }
                    },
                    else => return error.UnkInst,
                },
                0b0000011 => switch (inst.funct3()) {
                    0b000 => { // LB
                        const imm: u32 = @bitCast(inst.signed_i_imm());
                        const v: i8 = @bitCast(try self.mem.read(u8, self.regs[inst.rs1()] +% imm));
                        self.regs[inst.rd()] = @bitCast(@as(i32, v));
                        self.pc +%= 4;
                    },
                    0b001 => { // LH
                        const imm: u32 = @bitCast(inst.signed_i_imm());
                        const v: i16 = @bitCast(try self.mem.read(u16, self.regs[inst.rs1()] +% imm));
                        self.regs[inst.rd()] = @bitCast(@as(i32, v));
                        self.pc +%= 4;
                    },
                    0b010 => { // LW
                        const imm: u32 = @bitCast(inst.signed_i_imm());
                        self.regs[inst.rd()] = try self.mem.read(u32, self.regs[inst.rs1()] +% imm);
                        self.pc +%= 4;
                    },
                    0b100 => { // LBU
                        const imm: u32 = @bitCast(inst.signed_i_imm());
                        self.regs[inst.rd()] = @as(u32, try self.mem.read(u8, self.regs[inst.rs1()] +% imm));
                        self.pc +%= 4;
                    },
                    0b101 => { // LHU
                        const imm: u32 = @bitCast(inst.signed_i_imm());
                        self.regs[inst.rd()] = @as(u32, try self.mem.read(u16, self.regs[inst.rs1()] +% imm));
                        self.pc +%= 4;
                    },
                    else => return error.UnkInst,
                },
                0b0100011 => switch (inst.funct3()) {
                    0b000 => { // SB
                        const imm: u32 = @bitCast(inst.signed_s_imm());
                        try self.mem.write(u8, self.regs[inst.rs1()] +% imm, @truncate(self.regs[inst.rs2()]));
                        self.pc +%= 4;
                    },
                    0b001 => { // SH
                        const imm: u32 = @bitCast(inst.signed_s_imm());
                        try self.mem.write(u16, self.regs[inst.rs1()] +% imm, @truncate(self.regs[inst.rs2()]));
                        self.pc +%= 4;
                    },
                    0b010 => { // SW
                        const imm: u32 = @bitCast(inst.signed_s_imm());
                        try self.mem.write(u32, self.regs[inst.rs1()] +% imm, self.regs[inst.rs2()]);
                        self.pc +%= 4;
                    },
                    else => return error.UnkInst,
                },
                0b0010011 => switch (inst.funct3()) {
                    0b000 => { // ADDI
                        const imm: u32 = @bitCast(inst.signed_i_imm());
                        self.regs[inst.rd()] = self.regs[inst.rs1()] +% imm;
                        self.pc +%= 4;
                    },
                    0b010 => { // SLTI
                        const rs1: i32 = @bitCast(self.regs[inst.rs1()]);
                        self.regs[inst.rd()] = if (rs1 < inst.signed_i_imm()) 1 else 0;
                        self.pc +%= 4;
                    },
                    0b011 => { // SLTIU
                        const rs1 = self.regs[inst.rs1()];
                        const imm: u32 = @bitCast(inst.signed_i_imm());
                        self.regs[inst.rd()] = if (rs1 < imm) 1 else 0;
                        self.pc +%= 4;
                    },
                    0b100 => { // XORI
                        const imm: u32 = @bitCast(inst.signed_i_imm());
                        self.regs[inst.rd()] = self.regs[inst.rs1()] ^ imm;
                        self.pc +%= 4;
                    },
                    0b110 => { // ORI
                        const imm: u32 = @bitCast(inst.signed_i_imm());
                        self.regs[inst.rd()] = self.regs[inst.rs1()] | imm;
                        self.pc +%= 4;
                    },
                    0b111 => { // ANDI
                        const imm: u32 = @bitCast(inst.signed_i_imm());
                        self.regs[inst.rd()] = self.regs[inst.rs1()] & imm;
                        self.pc +%= 4;
                    },
                    0b001 => switch (inst.funct7()) {
                        0b0000000 => { // SLLI
                            self.regs[inst.rd()] = self.regs[inst.rs1()] << inst.shamt();
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                    0b101 => switch (inst.funct7()) {
                        0b0000000 => { // SRLI
                            self.regs[inst.rd()] = self.regs[inst.rs1()] >> inst.shamt();
                            self.pc +%= 4;
                        },
                        0b0100000 => { // SRAI
                            const rs1: i32 = @bitCast(self.regs[inst.rs1()]);
                            self.regs[inst.rd()] = @bitCast(rs1 >> inst.shamt());
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                },
                0b0110011 => switch (inst.funct3()) {
                    0b000 => switch (inst.funct7()) {
                        0b0000000 => { // ADD
                            self.regs[inst.rd()] = self.regs[inst.rs1()] +% self.regs[inst.rs2()];
                            self.pc +%= 4;
                        },
                        0b0100000 => { // SUB
                            self.regs[inst.rd()] = self.regs[inst.rs1()] -% self.regs[inst.rs2()];
                            self.pc +%= 4;
                        },
                        0b0000001 => { // MUL
                            self.regs[inst.rd()] = self.regs[inst.rs1()] *% self.regs[inst.rs2()];
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                    0b001 => switch (inst.funct7()) {
                        0b0000000 => { // SLL
                            self.regs[inst.rd()] = self.regs[inst.rs1()] << @truncate(self.regs[inst.rs2()]);
                            self.pc +%= 4;
                        },
                        0b0000001 => { // MULH
                            const rs1: i32 = @bitCast(self.regs[inst.rs1()]);
                            const rs2: i32 = @bitCast(self.regs[inst.rs2()]);
                            const rd: u64 = @bitCast(@as(i64, rs1) * @as(i64, rs2));
                            self.regs[inst.rd()] = @truncate(rd >> 32);
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                    0b010 => switch (inst.funct7()) {
                        0b0000000 => { // SLT
                            const rs1: i32 = @bitCast(self.regs[inst.rs1()]);
                            const rs2: i32 = @bitCast(self.regs[inst.rs2()]);
                            self.regs[inst.rd()] = if (rs1 < rs2) 1 else 0;
                            self.pc +%= 4;
                        },
                        0b0000001 => { // MULHSU
                            const rs1: i32 = @bitCast(self.regs[inst.rs1()]);
                            const rd: u64 = @bitCast(@as(i64, rs1) * @as(i64, self.regs[inst.rs2()]));
                            self.regs[inst.rd()] = @truncate(rd >> 32);
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                    0b011 => switch (inst.funct7()) {
                        0b0000000 => { // SLTU
                            const rs1 = self.regs[inst.rs1()];
                            const rs2 = self.regs[inst.rs2()];
                            self.regs[inst.rd()] = if (rs1 < rs2) 1 else 0;
                            self.pc +%= 4;
                        },
                        0b0000001 => { // MULHU
                            const rs1 = self.regs[inst.rs1()];
                            const rs2 = self.regs[inst.rs2()];
                            self.regs[inst.rd()] = @truncate(@as(u64, rs1) * @as(u64, rs2) >> 32);
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                    0b100 => switch (inst.funct7()) {
                        0b0000000 => { // XOR
                            self.regs[inst.rd()] = self.regs[inst.rs1()] ^ self.regs[inst.rs2()];
                            self.pc +%= 4;
                        },
                        0b0000001 => { // DIV
                            const rs1: i32 = @bitCast(self.regs[inst.rs1()]);
                            const rs2: i32 = @bitCast(self.regs[inst.rs2()]);
                            self.regs[inst.rd()] = if (rs2 == 0) // div by zero
                                @bitCast(@as(i32, -1))
                            else if (rs1 == std.math.minInt(i32) and rs2 == @as(i32, -1)) // overflow
                                @bitCast(@as(i32, std.math.minInt(i32)))
                            else
                                @bitCast(@divTrunc(rs1, rs2));
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                    0b101 => switch (inst.funct7()) {
                        0b0000000 => { // SRL
                            self.regs[inst.rd()] = self.regs[inst.rs1()] >> @truncate(self.regs[inst.rs2()]);
                            self.pc +%= 4;
                        },
                        0b0100000 => { // SRA
                            const rs1: i32 = @bitCast(self.regs[inst.rs1()]);
                            self.regs[inst.rd()] = @bitCast(rs1 >> @truncate(self.regs[inst.rs2()]));
                            self.pc +%= 4;
                        },
                        0b0000001 => { // DIVU
                            const rs2 = self.regs[inst.rs2()];
                            self.regs[inst.rd()] = if (rs2 == 0) // div by zero
                                ~@as(u32, 0)
                            else
                                self.regs[inst.rs1()] / rs2;
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                    0b110 => switch (inst.funct7()) {
                        0b0000000 => { // OR
                            self.regs[inst.rd()] = self.regs[inst.rs1()] | self.regs[inst.rs2()];
                            self.pc +%= 4;
                        },
                        0b0000001 => { // REM
                            const rs1: i32 = @bitCast(self.regs[inst.rs1()]);
                            const rs2: i32 = @bitCast(self.regs[inst.rs2()]);
                            self.regs[inst.rd()] = if (rs2 == 0) // div by zero
                                @bitCast(rs1)
                            else if (rs1 == std.math.minInt(i32) and rs2 == @as(i32, -1)) // overflow
                                @as(u32, 0)
                            else
                                @bitCast(@rem(rs1, rs2));
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                    0b111 => switch (inst.funct7()) {
                        0b0000000 => { // AND
                            self.regs[inst.rd()] = self.regs[inst.rs1()] & self.regs[inst.rs2()];
                            self.pc +%= 4;
                        },
                        0b0000001 => { // REMU
                            const rs2 = self.regs[inst.rs2()];
                            const rs1 = self.regs[inst.rs1()];
                            self.regs[inst.rd()] = if (rs2 == 0) // div by zero
                                rs1
                            else
                                rs1 % rs2;
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                },
                0b1110011 => switch (inst.funct3()) {
                    0b000 => switch (inst.funct12()) {
                        0b000000000000 => { // ECALL
                            return error.Ecall;
                        },
                        0b000000000001 => { // EBREAK
                            log_debug(@src(), "Unimplemented ebreak", .{});
                            self.pc +%= 4;
                        },
                        0b000000000010 => { // URET
                            log_debug(@src(), "Unimplemented uret", .{});
                            self.pc +%= 4;
                        },
                        0b000100000010 => { // SRET
                            log_debug(@src(), "Unimplemented sret", .{});
                            self.pc +%= 4;
                        },
                        0b001100000010 => { // MRET
                            var mstatus: MStatus = @bitCast(self.csr.regs[Csr.MSTATUS]);
                            self.priv_mode = @enumFromInt(mstatus.mpp);
                            mstatus.mie = mstatus.mpie;
                            self.pc = self.csr.regs[Csr.MEPC];
                        },
                        0b000100000101 => { // WFI
                            log_debug(@src(), "Unimplemented wfi", .{});
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                    0b001 => { // CSRRW,
                        if (inst.rd() != 0) {
                            const v = try self.csr.read(self.priv_mode, inst.csr());
                            self.regs[inst.rd()] = v;
                        }
                        try self.csr.write(self.priv_mode, inst.csr(), self.regs[inst.rs1()]);
                        self.pc +%= 4;
                    },
                    0b010 => { // CSRRS,
                        const csr = inst.csr();
                        const v = try self.csr.read(self.priv_mode, csr);
                        self.regs[inst.rd()] = v;
                        const inst_rs1 = inst.rs1();
                        if (inst_rs1 != 0) {
                            try self.csr.write(self.priv_mode, csr, v | self.regs[inst_rs1]);
                        }
                        self.pc +%= 4;
                    },
                    0b011 => { // CSRRC,
                        const csr = inst.csr();
                        const v = try self.csr.read(self.priv_mode, csr);
                        self.regs[inst.rd()] = v;
                        const inst_rs1 = inst.rs1();
                        if (inst_rs1 != 0) {
                            try self.csr.write(self.priv_mode, csr, v & ~self.regs[inst_rs1]);
                        }
                        self.pc +%= 4;
                    },
                    0b101 => { // CSRRWI,
                        const csr = inst.csr();
                        if (inst.rd() != 0) {
                            const v = try self.csr.read(self.priv_mode, csr);
                            self.regs[inst.rd()] = v;
                        }
                        const uimm = inst.csr_imm();
                        try self.csr.write(self.priv_mode, csr, uimm);
                        self.pc +%= 4;
                    },
                    0b110 => { // CSRRSI,
                        const csr = inst.csr();
                        const v = try self.csr.read(self.priv_mode, csr);
                        self.regs[inst.rd()] = v;
                        const uimm = inst.csr_imm();
                        if (uimm != 0) {
                            try self.csr.write(self.priv_mode, csr, v | uimm);
                        }
                        self.pc +%= 4;
                    },
                    0b111 => { // CSRRCI,
                        const csr = inst.csr();
                        const v = try self.csr.read(self.priv_mode, csr);
                        self.regs[inst.rd()] = v;
                        const uimm = inst.csr_imm();
                        if (uimm != 0) {
                            try self.csr.write(self.priv_mode, csr, v & ~uimm);
                        }
                        self.pc +%= 4;
                    },
                    else => return error.UnkInst,
                },
                0b0101111 => switch (inst.funct3()) {
                    // TODO miss-aligned address exception
                    0b10 => switch (inst.funct5()) {
                        0b00010 => { // LR_W
                            const rs1 = self.regs[inst.rs1()];
                            const v = try self.mem.read(u32, rs1);
                            self.regs[inst.rd()] = v;
                            try self.mem.reserve(rs1);
                            self.pc +%= 4;
                        },
                        0b00011 => { // SC_W
                            const rs1 = self.regs[inst.rs1()];
                            const valid = try self.mem.validate_reservation(rs1);
                            if (valid) {
                                const rs2 = self.regs[inst.rs2()];
                                try self.mem.write(u32, rs1, rs2);
                                self.regs[inst.rd()] = 0;
                            } else {
                                self.regs[inst.rd()] = 1;
                            }
                            self.pc +%= 4;
                        },
                        0b00001 => { // AMOSWAP_W
                            const rs1 = self.regs[inst.rs1()];
                            var v = try self.mem.read(u32, rs1);
                            self.regs[inst.rd()] = v;
                            v = self.regs[inst.rs2()];
                            try self.mem.write(u32, rs1, v);
                            self.pc +%= 4;
                        },
                        0b00000 => { // AMOADD_W
                            const rs1 = self.regs[inst.rs1()];
                            var v = try self.mem.read(u32, rs1);
                            self.regs[inst.rd()] = v;
                            v = v +% self.regs[inst.rs2()];
                            try self.mem.write(u32, rs1, v);
                            self.pc +%= 4;
                        },
                        0b00100 => { // AMOXOR_W
                            const rs1 = self.regs[inst.rs1()];
                            var v = try self.mem.read(u32, rs1);
                            self.regs[inst.rd()] = v;
                            v = v ^ self.regs[inst.rs2()];
                            try self.mem.write(u32, rs1, v);
                            self.pc +%= 4;
                        },
                        0b01100 => { // AMOAND_W
                            const rs1 = self.regs[inst.rs1()];
                            var v = try self.mem.read(u32, rs1);
                            self.regs[inst.rd()] = v;
                            v = v & self.regs[inst.rs2()];
                            try self.mem.write(u32, rs1, v);
                            self.pc +%= 4;
                        },
                        0b01000 => { // AMOOR_W
                            const rs1 = self.regs[inst.rs1()];
                            var v = try self.mem.read(u32, rs1);
                            self.regs[inst.rd()] = v;
                            v = v | self.regs[inst.rs2()];
                            try self.mem.write(u32, rs1, v);
                            self.pc +%= 4;
                        },
                        0b10000 => { // AMOMIN_W
                            const rs1 = self.regs[inst.rs1()];
                            var v: i32 = @bitCast(try self.mem.read(u32, rs1));
                            self.regs[inst.rd()] = @bitCast(v);
                            const rs2: i32 = @bitCast(self.regs[inst.rs2()]);
                            v = @min(v, rs2);
                            try self.mem.write(u32, rs1, @bitCast(v));
                            self.pc +%= 4;
                        },
                        0b10100 => { // AMOMAX_W
                            const rs1 = self.regs[inst.rs1()];
                            var v: i32 = @bitCast(try self.mem.read(u32, rs1));
                            self.regs[inst.rd()] = @bitCast(v);
                            const rs2: i32 = @bitCast(self.regs[inst.rs2()]);
                            v = @max(v, rs2);
                            try self.mem.write(u32, rs1, @bitCast(v));
                            self.pc +%= 4;
                        },
                        0b11000 => { // AMOMINU_W
                            const rs1 = self.regs[inst.rs1()];
                            var v = try self.mem.read(u32, rs1);
                            self.regs[inst.rd()] = v;
                            v = @min(v, self.regs[inst.rs2()]);
                            try self.mem.write(u32, rs1, v);
                            self.pc +%= 4;
                        },
                        0b11100 => { // AMOMAXU_W
                            const rs1 = self.regs[inst.rs1()];
                            var v = try self.mem.read(u32, rs1);
                            self.regs[inst.rd()] = v;
                            v = @max(v, self.regs[inst.rs2()]);
                            try self.mem.write(u32, rs1, v);
                            self.pc +%= 4;
                        },
                        else => return error.UnkInst,
                    },
                    else => return error.UnkInst,
                },
                0b0001111 => switch (inst.funct3()) {
                    0b000 => { // FENCE
                        self.pc +%= 4;
                    },
                    0b001 => { // FENCEI
                        self.pc +%= 4;
                    },
                    else => return error.UnkInst,
                },
                else => return error.UnkInst,
            }
            self.regs[0] = 0;
        }
    };
}

test "inst" {
    const inst = Inst.init(0);
    _ = inst;
}
