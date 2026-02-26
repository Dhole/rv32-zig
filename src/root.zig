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

pub const Memory = struct {
    allocator: Allocator,
    rom_offset: u32,
    rom: []u8,
    ram_offset: u32,
    ram: []u8,

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
        };
    }

    pub fn read(self: *Self, comptime T: type, addr: u32) T {
        if (self.rom_offset <= addr and addr < self.rom_offset + self.rom.len) {
            return buf_read(T, self.rom, addr - self.rom_offset);
        } else if (self.ram_offset <= addr and addr < self.ram_offset + self.ram.len) {
            return buf_read(T, self.ram, addr - self.ram_offset);
        }
        @panic("TODO: invalid read");
    }

    pub fn write(self: *Self, comptime T: type, addr: u32, v: T) void {
        if (self.ram_offset <= addr and addr < self.ram_offset + self.ram.len) {
            return buf_write(T, self.ram, addr - self.ram_offset, v);
        }
        debug.panic("TODO: invalid write at addr {x:08}", .{addr});
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.rom);
        self.allocator.free(self.ram);
    }
};

const CSR_MEPC: u32 = 0x341;

pub const Trap = enum {
    invalid_inst,
    ecall,
};

pub const Cpu = struct {
    regs: [32]u32,
    pc: u32,
    csr: [4096]u32,
    mem: Memory,

    const Self = @This();

    pub fn init(mem: Memory) Self {
        return Self{
            .regs = .{0} ** 32,
            .pc = 0,
            .csr = [_]u32{0} ** 4096,
            .mem = mem,
        };
    }

    pub fn deinit(self: *Self) void {
        self.mem.deinit();
    }

    pub fn exec(self: *Self, inst: Inst) ?Trap {
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
                else => return .invalid_inst,
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
                else => return .invalid_inst,
            },
            0b0000011 => switch (inst.funct3()) {
                0b000 => { // LB
                    const imm: u32 = @bitCast(inst.signed_i_imm());
                    const v: i8 = @bitCast(self.mem.read(u8, self.regs[inst.rs1()] +% imm));
                    self.regs[inst.rd()] = @bitCast(@as(i32, v));
                    self.pc +%= 4;
                },
                0b001 => { // LH
                    const imm: u32 = @bitCast(inst.signed_i_imm());
                    const v: i16 = @bitCast(self.mem.read(u16, self.regs[inst.rs1()] +% imm));
                    self.regs[inst.rd()] = @bitCast(@as(i32, v));
                    self.pc +%= 4;
                },
                0b010 => { // LW
                    const imm: u32 = @bitCast(inst.signed_i_imm());
                    self.regs[inst.rd()] = self.mem.read(u32, self.regs[inst.rs1()] +% imm);
                    self.pc +%= 4;
                },
                0b100 => { // LBU
                    const imm: u32 = @bitCast(inst.signed_i_imm());
                    self.regs[inst.rd()] = @as(u32, self.mem.read(u8, self.regs[inst.rs1()] +% imm));
                    self.pc +%= 4;
                },
                0b101 => { // LHU
                    const imm: u32 = @bitCast(inst.signed_i_imm());
                    self.regs[inst.rd()] = @as(u32, self.mem.read(u16, self.regs[inst.rs1()] +% imm));
                    self.pc +%= 4;
                },
                else => return .invalid_inst,
            },
            0b0100011 => switch (inst.funct3()) {
                0b000 => { // SB
                    const imm: u32 = @bitCast(inst.signed_s_imm());
                    self.mem.write(u8, self.regs[inst.rs1()] +% imm, @truncate(self.regs[inst.rs2()]));
                    self.pc +%= 4;
                },
                0b001 => { // SH
                    const imm: u32 = @bitCast(inst.signed_s_imm());
                    self.mem.write(u16, self.regs[inst.rs1()] +% imm, @truncate(self.regs[inst.rs2()]));
                    self.pc +%= 4;
                },
                0b010 => { // SW
                    const imm: u32 = @bitCast(inst.signed_s_imm());
                    self.mem.write(u32, self.regs[inst.rs1()] +% imm, self.regs[inst.rs2()]);
                    self.pc +%= 4;
                },
                else => return .invalid_inst,
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
                    else => return .invalid_inst,
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
                    else => return .invalid_inst,
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
                    else => return .invalid_inst,
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
                    else => return .invalid_inst,
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
                    else => return .invalid_inst,
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
                    else => return .invalid_inst,
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
                    else => return .invalid_inst,
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
                    else => return .invalid_inst,
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
                    else => return .invalid_inst,
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
                    else => return .invalid_inst,
                },
            },
            0b1110011 => switch (inst.funct3()) {
                0b000 => switch (inst.funct12()) {
                    0b000000000000 => { // ECALL
                        return .ecall;
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
                        self.pc = self.csr[CSR_MEPC];
                    },
                    0b000100000101 => { // WFI
                        log_debug(@src(), "Unimplemented wfi", .{});
                        self.pc +%= 4;
                    },
                    else => return .invalid_inst,
                },
                0b001 => { // CSRRW,
                    if (inst.rd() != 0) {
                        const v = self.csr[inst.csr()];
                        self.regs[inst.rd()] = v;
                    }
                    self.csr[inst.csr()] = self.regs[inst.rs1()];
                    self.pc +%= 4;
                },
                0b010 => { // CSRRS,
                    const v = self.csr[inst.csr()];
                    self.regs[inst.rd()] = v;
                    self.csr[inst.csr()] = v | self.regs[inst.rs1()];
                    self.pc +%= 4;
                },
                0b011 => { // CSRRC,
                    const v = self.csr[inst.csr()];
                    self.regs[inst.rd()] = v;
                    self.csr[inst.csr()] = v & ~self.regs[inst.rs1()];
                    self.pc +%= 4;
                },
                0b101 => { // CSRRWI,
                    if (inst.rd() != 0) {
                        const v = self.csr[inst.csr()];
                        self.regs[inst.rd()] = v;
                    }
                    self.csr[inst.csr()] = inst.csr();
                    self.pc +%= 4;
                },
                0b110 => { // CSRRSI,
                    const v = self.csr[inst.csr()];
                    self.regs[inst.rd()] = v;
                    const uimm = inst.csr_imm();
                    if (uimm != 0) {
                        self.csr[inst.csr()] = v | uimm;
                    }
                    self.pc +%= 4;
                },
                0b111 => { // CSRRCI,
                    const v = self.csr[inst.csr()];
                    self.regs[inst.rd()] = v;
                    const uimm = inst.csr_imm();
                    if (uimm != 0) {
                        self.csr[inst.csr()] = v & ~uimm;
                    }
                    self.pc +%= 4;
                },
                else => return .invalid_inst,
            },
            0b0001111 => switch (inst.funct3()) {
                0b000 => { // FENCE
                    self.pc +%= 4;
                },
                0b001 => { // FENCEI
                    self.pc +%= 4;
                },
                else => return .invalid_inst,
            },
            else => return .invalid_inst,
        }
        self.regs[0] = 0;
        return null;
    }
};

test "inst" {
    const inst = Inst.init(0);
    _ = inst;
}
