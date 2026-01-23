const std = @import("std");
const AST = @import("ast.zig");

pub const OpCode = enum(u8) {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    SetLocal,
    GetGlobal,
    DefineGlobal,
    Equal,
    NotEqual,
    Greater,
    GreaterNum,
    GreaterEqual,
    GreaterEqualNum,
    Less,
    LessNum,
    LessEqual,
    LessEqualNum,
    Add,
    AddNum,
    Subtract,
    SubtractNum,
    Multiply,
    MultiplyNum,
    Divide,
    DivideNum,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    GetAttr,
    Import,
    Return,
    BuildList,
    BuildDict,
    GetSubscript,
    SetSubscript,
    ListLen,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: std.ArrayList(AST.Value),

    pub fn init() Chunk {
        return .{
            .code = std.ArrayList(u8){},
            .constants = std.ArrayList(AST.Value){},
        };
    }

    pub fn deinit(self: *Chunk, allocator: std.mem.Allocator) void {
        self.code.deinit(allocator);
        self.constants.deinit(allocator);
    }

    pub fn writeOp(self: *Chunk, allocator: std.mem.Allocator, op: OpCode) !void {
        try self.code.append(allocator, @intFromEnum(op));
    }

    pub fn writeU8(self: *Chunk, allocator: std.mem.Allocator, byte: u8) !void {
        try self.code.append(allocator, byte);
    }

    pub fn writeU16(self: *Chunk, allocator: std.mem.Allocator, value: u16) !void {
        try self.code.append(allocator, @as(u8, @intCast((value >> 8) & 0xff)));
        try self.code.append(allocator, @as(u8, @intCast(value & 0xff)));
    }

    pub fn addConstant(self: *Chunk, allocator: std.mem.Allocator, value: AST.Value) !u16 {
        const idx = self.constants.items.len;
        try self.constants.append(allocator, value);
        return @as(u16, @intCast(idx));
    }
};

pub const BytecodeFunction = struct {
    name: []const u8,
    chunk: Chunk,
    arity: u8,
    locals_count: usize,
    this_slot: i32,
    globals: ?*std.StringHashMap(AST.Value),
};
