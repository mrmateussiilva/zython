const std = @import("std");

pub const Value = union(enum) {
    Nil: void,
    Boolean: bool,
    Number: f64,
    String: []const u8,
    
    pub fn toString(self: Value, allocator: std.mem.Allocator) ![]u8 {
        switch (self) {
            .Nil => return try allocator.dupe(u8, "None"),
            .Boolean => |b| return try allocator.dupe(u8, if (b) "True" else "False"),
            .Number => |n| return std.fmt.allocPrint(allocator, "{d}", .{n}),
            .String => |s| return try allocator.dupe(u8, s),
        }
    }
};

pub const BinaryOp = enum {
    Add, Sub, Mul, Div, Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual
};

pub const UnaryOp = enum {
    Negate, Not
};

pub const Expr = union(enum) {
    Binary: struct {
        left: *const Expr,
        op: BinaryOp,
        right: *const Expr,
    },
    Unary: struct {
        op: UnaryOp,
        right: *const Expr,
    },
    Literal: Value,
    Grouping: *const Expr,
    Variable: []const u8,
};

pub const Stmt = union(enum) {
    Expression: struct {
        expression: Expr,
    },
    Print: struct {
        expression: Expr,
    },
    Var: struct {
        name: []const u8,
        initializer: Expr,
    },
    Block: struct {
        statements: []const Stmt,
    },
};
