const std = @import("std");

// Forward declarations for runtime structures
pub const LoxClass = struct {
    name: []const u8,
    methods: std.StringHashMap(Value),
};

pub const LoxInstance = struct {
    klass: *LoxClass,
    fields: std.StringHashMap(Value),
};

pub const LoxList = struct {
    elements: std.ArrayList(Value),
};

pub const Value = union(enum) {
    Nil: void,
    Boolean: bool,
    Number: f64,
    String: []const u8,
    Function: struct {
        name: []const u8,
        params: []const []const u8,
        body: []const Stmt,
        closure: ?*anyopaque,
    },
    Class: *LoxClass,
    Instance: *LoxInstance,
    List: *LoxList,
    NativeFunction: *const fn(allocator: std.mem.Allocator, args: []const Value) InterpreterError!Value,
    
    pub fn toString(self: Value, allocator: std.mem.Allocator) ![]u8 {
        switch (self) {
            .Nil => return try allocator.dupe(u8, "None"),
            .Boolean => |b| return try allocator.dupe(u8, if (b) "True" else "False"),
            .Number => |n| return std.fmt.allocPrint(allocator, "{d}", .{n}),
            .String => |s| return try allocator.dupe(u8, s),
            .Function => |f| return try std.fmt.allocPrint(allocator, "<function {s}>", .{f.name}),
            .NativeFunction => |_| return try allocator.dupe(u8, "<native fn>"),
            .Class => |c| return try std.fmt.allocPrint(allocator, "<class {s}>", .{c.name}),
            .Instance => |i| return try std.fmt.allocPrint(allocator, "<{s} instance>", .{i.klass.name}),
            .List => |l| {
                var list_str = std.ArrayList(u8){};
                defer list_str.deinit(allocator);
                try list_str.append(allocator, '[');
                for (l.elements.items, 0..) |elem, i| {
                    const s = try elem.toString(allocator);
                    try list_str.appendSlice(allocator, s);
                    if (i < l.elements.items.len - 1) {
                        try list_str.appendSlice(allocator, ", ");
                    }
                }
                try list_str.append(allocator, ']');
                return list_str.toOwnedSlice(allocator);
            },
        }
    }
};

pub const InterpreterError = error{
    RuntimeError,
    TypeError,
    UndefinedVariable,
    Return, 
    OutOfMemory,
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
    Get: struct {
        object: *const Expr,
        name: []const u8,
    },
    Set: struct {
        object: *const Expr,
        name: []const u8,
        value: *const Expr,
    },
    This: struct {
        keyword: []const u8,
    },
    ListLiteral: struct {
        elements: []const Expr,
    },
    Subscript: struct {
        value: *const Expr,
        index: *const Expr,
    },
    SetSubscript: struct {
        object: *const Expr,
        index: *const Expr,
        value: *const Expr,
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
    For: struct {
        variable: []const u8,
        iterable: Expr,
        body: *const Stmt,
    },
    Function: struct {
        name: []const u8,
        params: []const []const u8,
        body: []const Stmt,
    },
    Class: struct {
        name: []const u8,
        methods: []const Stmt,
    },
    Return: struct {
        value: ?Expr,
    },
};
