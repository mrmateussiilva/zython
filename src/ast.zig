const std = @import("std");

pub const Value = union(enum) {
    Nil: void,
    Boolean: bool,
    Number: f64,
    String: []const u8,
    Function: struct {
        name: []const u8,
        params: [][]const u8,
        body: []const Stmt,
    },
    
    pub fn toString(self: Value, allocator: std.mem.Allocator) ![]u8 {
        switch (self) {
            .Nil => return try allocator.dupe(u8, "None"),
            .Boolean => |b| return try allocator.dupe(u8, if (b) "True" else "False"),
            .Number => |n| return std.fmt.allocPrint(allocator, "{d}", .{n}),
            .String => |s| return try allocator.dupe(u8, s),
            .Function => |f| return try std.fmt.allocPrint(allocator, "<function {s}>", .{f.name}),
        }
    }
};

pub const BinaryOp = enum {
    Add, Sub, Mul, Div, Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual
};

pub const UnaryOp = enum {
    Negate, Not
};

pub const LogicalOp = enum {
    And, Or
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
    Logical: struct {
        left: *const Expr,
        op: LogicalOp,
        right: *const Expr,
    },
    Call: struct {
        callee: *const Expr,
        arguments: []const Expr,
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
    If: struct {
        condition: Expr,
        then_branch: *const Stmt,
        else_branch: ?*const Stmt,
    },
    While: struct {
        condition: Expr,
        body: *const Stmt,
    },
    Function: struct {
        name: []const u8,
        params: [][]const u8,
        body: []const Stmt,
    },
    Return: struct {
        value: ?Expr,
    },
};
