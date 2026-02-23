const std = @import("std");
const Writer = std.Io.Writer;

const root = @import("root.zig");
const Inst = root.Inst;

const reg = [32][]const u8{
    "zero", // x0, Hard-wired zero
    "ra", // x1, Return address
    "sp", // x2, Stack pointer
    "gp", // x3, Global pointer
    "tp", // x4, Thread pointer
    "t0", // x5, Temporary/alternate link register
    "t1", "t2", // x6-7, Temporaries
    "s0", // x8, Saved register/Frame pointer
    "s1", // x9, Saved register
    "a0", "a1", // x10-11, Function arguments/return values
    "a2", "a3", "a4", "a5", "a6", "a7", // x12-17, Function arguments
    "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", // x18-x27, Saved registers
    "t3", "t4", "t5", "t6", // x28-x31, Temporaries
};

const Opcode = enum {
    const Self = @This();

    // I
    LUI,
    AUIPC,
    JAL,
    JALR,
    BEQ,
    BNE,
    BLT,
    BGE,
    BLTU,
    BGEU,
    LB,
    LH,
    LW,
    LBU,
    LHU,
    SB,
    SH,
    SW,
    ADDI,
    SLTI,
    SLTIU,
    XORI,
    ORI,
    ANDI,
    SLLI,
    SRLI,
    SRAI,
    ADD,
    SUB,
    SLL,
    SLT,
    SLTU,
    XOR,
    SRL,
    SRA,
    OR,
    AND,
    FENCE,
    ECALL,
    EBREAK,

    // "Zicsr", Control and Status Register (CSR) Instructions, Version 2.0
    CSRRW,
    CSRRS,
    CSRRC,
    CSRRWI,
    CSRRSI,
    CSRRCI,

    // "Zifencei", Instruction-Fetch Fence, Version 2.0
    FENCEI,

    // Privileged Instruction Set
    URET,
    SRET,
    MRET,
    WFI,

    fn str(self: Self) []const u8 {
        return switch (self) {
            .LUI => "lui",
            .AUIPC => "auipc",
            .JAL => "jal",
            .JALR => "jalr",
            .BEQ => "beq",
            .BNE => "bne",
            .BLT => "blt",
            .BGE => "bge",
            .BLTU => "bltu",
            .BGEU => "bgeu",
            .LB => "lb",
            .LH => "lh",
            .LW => "lw",
            .LBU => "lbu",
            .LHU => "lhu",
            .SB => "sb",
            .SH => "sh",
            .SW => "sw",
            .ADDI => "addi",
            .SLTI => "slti",
            .SLTIU => "sltiu",
            .XORI => "xori",
            .ORI => "ori",
            .ANDI => "andi",
            .SLLI => "slli",
            .SRLI => "srli",
            .SRAI => "srai",
            .ADD => "add",
            .SUB => "sub",
            .SLL => "sll",
            .SLT => "slt",
            .SLTU => "sltu",
            .XOR => "xor",
            .SRL => "srl",
            .SRA => "sra",
            .OR => "or",
            .AND => "and",
            .FENCE => "fence",
            .ECALL => "ecall",
            .EBREAK => "ebreak",
            .CSRRW => "csrrw",
            .CSRRS => "csrrs",
            .CSRRC => "csrrc",
            .CSRRWI => "csrrwi",
            .CSRRSI => "csrrsi",
            .CSRRCI => "csrrci",
            .FENCEI => "fencei",
            .URET => "uret",
            .SRET => "sret",
            .MRET => "mret",
            .WFI => "wfi",
        };
    }
};

pub fn inst_op(inst: Inst) ?Opcode {
    return switch (inst.opcode()) {
        0b0110111 => .LUI,
        0b0010111 => .AUIPC,
        0b1101111 => .JAL,
        0b1100111 => switch (inst.funct3()) {
            0b000 => .JALR,
            else => null,
        },
        0b1100011 => switch (inst.funct3()) {
            0b000 => .BEQ,
            0b001 => .BNE,
            0b100 => .BLT,
            0b101 => .BGE,
            0b110 => .BLTU,
            0b111 => .BGEU,
            else => null,
        },
        0b0000011 => switch (inst.funct3()) {
            0b000 => .LB,
            0b001 => .LH,
            0b010 => .LW,
            0b100 => .LBU,
            0b101 => .LHU,
            else => null,
        },
        0b0100011 => switch (inst.funct3()) {
            0b000 => .SB,
            0b001 => .SH,
            0b010 => .SW,
            else => null,
        },
        0b0010011 => switch (inst.funct3()) {
            0b000 => .ADDI,
            0b010 => .SLTI,
            0b011 => .SLTIU,
            0b100 => .XORI,
            0b110 => .ORI,
            0b111 => .ANDI,
            0b001 => switch (inst.funct7()) {
                0b0000000 => .SLLI,
                else => null,
            },
            0b101 => switch (inst.funct7()) {
                0b0000000 => .SRLI,
                0b0100000 => .SRAI,
                else => null,
            },
        },
        0b0110011 => switch (inst.funct3()) {
            0b000 => switch (inst.funct7()) {
                0b0000000 => .ADD,
                0b0100000 => .SUB,
                else => null,
            },
            0b001 => switch (inst.funct7()) {
                0b0000000 => .SLL,
                else => null,
            },
            0b010 => switch (inst.funct7()) {
                0b0000000 => .SLT,
                else => null,
            },
            0b011 => switch (inst.funct7()) {
                0b0000000 => .SLTU,
                else => null,
            },
            0b100 => switch (inst.funct7()) {
                0b0000000 => .XOR,
                else => null,
            },
            0b101 => switch (inst.funct7()) {
                0b0000000 => .SRL,
                0b0100000 => .SRA,
                else => null,
            },
            0b110 => switch (inst.funct7()) {
                0b0000000 => .OR,
                else => null,
            },
            0b111 => switch (inst.funct7()) {
                0b0000000 => .AND,
                else => null,
            },
        },
        0b1110011 => switch (inst.funct3()) {
            0b000 => switch (inst.funct12()) {
                0b000000000000 => .ECALL,
                0b000000000001 => .EBREAK,
                0b000000000010 => .URET,
                0b000100000010 => .SRET,
                0b001100000010 => .MRET,
                0b000100000101 => .WFI,
                else => null,
            },
            0b001 => .CSRRW,
            0b010 => .CSRRS,
            0b011 => .CSRRC,
            0b101 => .CSRRWI,
            0b110 => .CSRRSI,
            0b111 => .CSRRCI,
            else => null,
        },
        0b0001111 => switch (inst.funct3()) {
            0b000 => .FENCE,
            0b001 => .FENCEI,
            else => null,
        },
        else => null,
    };
}

pub fn inst_format(w: *Writer, addr: u32, inst: Inst) !void {
    const op = inst_op(inst) orelse {
        try w.print("unk", .{});
        return;
    };
    try w.print("{s}", .{op.str()});
    switch (op) {
        // R-Type
        .ADD,
        .SUB,
        .SLL,
        .SLT,
        .SLTU,
        .XOR,
        .SRL,
        .SRA,
        .OR,
        .AND,
        => {
            try w.print(" {s}, {s}, {s}", .{ reg[inst.rd()], reg[inst.rs1()], reg[inst.rs2()] });
        },
        // I-Type
        .JALR, .LB, .LH, .LW, .LBU, .LHU, .ADDI, .SLTI, .SLTIU, .XORI, .ORI, .ANDI, .SLLI, .SRLI, .SRAI, .FENCE, .ECALL, .EBREAK, .CSRRW, .CSRRS, .CSRRC, .CSRRWI, .CSRRSI, .CSRRCI, .FENCEI, .URET, .SRET, .MRET, .WFI => {
            try w.print(" {s}, {s}, {}", .{ reg[inst.rd()], reg[inst.rs1()], inst.signed_i_imm() });
        },
        // S-Type
        .SB, .SH, .SW => {
            try w.print(" {s}, {}({s})", .{ reg[inst.rs2()], inst.signed_s_imm(), reg[inst.rs1()] });
        },
        // B-Type
        .BEQ, .BNE, .BLT, .BGE, .BLTU, .BGEU => {
            const imm: u32 = @bitCast(inst.signed_b_imm());
            const offset: u32 = addr +% imm;
            try w.print(" {s}, {s}, 0x{x}", .{ reg[inst.rs1()], reg[inst.rs2()], offset });
        },
        // U-Type
        .LUI, .AUIPC => {
            const imm = inst.u_imm() >> 12;
            try w.print(" {s}, 0x{x}", .{ reg[inst.rd()], imm });
        },
        // J-Type
        .JAL => {
            const imm: u32 = @bitCast(inst.signed_j_imm());
            const offset: u32 = addr +% imm;
            try w.print(" {s}, 0x{x}", .{ reg[inst.rd()], offset });
        },
    }
}

pub const InstFmt = struct {
    addr: u32,
    inst: Inst,

    pub fn format(self: InstFmt, w: *Writer) !void {
        return inst_format(w, self.addr, self.inst);
    }
};
