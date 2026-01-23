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

pub const ValueContext = struct {
    pub fn hash(self: @This(), s: Value) u64 {
        _ = self;
        var hasher = std.hash.Wyhash.init(0);
        switch (s) {
            .Nil => hasher.update("Nil"),
            .Boolean => |b| hasher.update(if (b) "True" else "False"),
            .Number => |n| hasher.update(std.mem.asBytes(&n)),
            .String => |str| hasher.update(str),
            .List => |l| hasher.update(std.mem.asBytes(&l)), 
            .Class => |c| hasher.update(std.mem.asBytes(&c)),
            .Instance => |i| hasher.update(std.mem.asBytes(&i)),
            .Function => |f| hasher.update(f.name),
            .BytecodeFunction => |f| hasher.update(std.mem.asBytes(&f)),
            .NativeFunction => |f| hasher.update(std.mem.asBytes(&f)),
            .File => |f| hasher.update(std.mem.asBytes(&f.handle)),
            .Dict => |d| hasher.update(std.mem.asBytes(&d)),
            .Module => |m| hasher.update(m.name),
        }
        return hasher.final();
    }
    pub fn eql(self: @This(), a: Value, b: Value) bool {
        _ = self;
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        switch (a) {
            .Nil => return true,
            .Boolean => return a.Boolean == b.Boolean,
            .Number => return a.Number == b.Number,
            .String => return std.mem.eql(u8, a.String, b.String),
            .List => return a.List == b.List,
            .Class => return a.Class == b.Class,
            .Instance => return a.Instance == b.Instance,
            .Dict => return a.Dict == b.Dict,
            .File => return a.File.handle == b.File.handle,
            .Module => return a.Module.exports == b.Module.exports, // Identity check on environment
            .Function => return false,
            .BytecodeFunction => return a.BytecodeFunction == b.BytecodeFunction,
            .NativeFunction => return false,
        }
    }
};

pub const LoxDict = std.HashMap(Value, Value, ValueContext, std.hash_map.default_max_load_percentage);

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
        locals_count: usize,
        this_slot: i32,
    },
    BytecodeFunction: *anyopaque,
    Class: *LoxClass,
    Instance: *LoxInstance,
    List: *LoxList,
    Dict: *LoxDict,
    File: std.fs.File,
    Module: struct {
        name: []const u8,
        exports: *anyopaque, // *Environment
    },
    NativeFunction: *const fn(allocator: std.mem.Allocator, args: []const Value) InterpreterError!Value,
    
    pub fn toString(self: Value, allocator: std.mem.Allocator) ![]u8 {
        switch (self) {
            .Nil => return try allocator.dupe(u8, "None"),
            .Boolean => |b| return try allocator.dupe(u8, if (b) "True" else "False"),
            .Number => |n| return std.fmt.allocPrint(allocator, "{d}", .{n}),
            .String => |s| return try allocator.dupe(u8, s),
            .Function => |f| return try std.fmt.allocPrint(allocator, "<function {s}>", .{f.name}),
            .BytecodeFunction => |_| return try allocator.dupe(u8, "<bc-fn>"),
            .NativeFunction => |_| return try allocator.dupe(u8, "<native fn>"),
            .Class => |c| return try std.fmt.allocPrint(allocator, "<class {s}>", .{c.name}),
            .Instance => |i| return try std.fmt.allocPrint(allocator, "<{s} instance>", .{i.klass.name}),
            .File => |_| return try allocator.dupe(u8, "<file>"),
            .Module => |m| return try std.fmt.allocPrint(allocator, "<module {s}>", .{m.name}),
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
            .Dict => |d| {
                var str = std.ArrayList(u8){};
                defer str.deinit(allocator);
                try str.append(allocator, '{');
                var iter = d.iterator();
                var first = true;
                while (iter.next()) |entry| {
                    if (!first) try str.appendSlice(allocator, ", ");
                    first = false;
                    const keyStr = try entry.key_ptr.toString(allocator);
                    const valStr = try entry.value_ptr.toString(allocator);
                    
                    // Add quotes if key is string? Python does 'key': val. 
                    // But toString already returns raw string content for .String...
                    // Let's rely on toString for now. Ideally toString for String should add quotes if it's debug print, but here it's raw.
                    // For dict output, we usually want repr(), but let's stick to toString.
                    if (entry.key_ptr.* == .String) try str.append(allocator, '\'');
                    try str.appendSlice(allocator, keyStr);
                    if (entry.key_ptr.* == .String) try str.append(allocator, '\'');
                    
                    try str.appendSlice(allocator, ": ");
                    
                    if (entry.value_ptr.* == .String) try str.append(allocator, '\'');
                    try str.appendSlice(allocator, valStr);
                    if (entry.value_ptr.* == .String) try str.append(allocator, '\'');
                }
                try str.append(allocator, '}');
                return str.toOwnedSlice(allocator);
            },
        }
    }
};

pub const InterpreterError = error{
    RuntimeError,
    TypeError,
    UndefinedVariable,
    Return, 
    ZythonException,
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
        depth: i32, // -1 => global
        slot: i32, // -1 => global
    },
    ListLiteral: struct {
        elements: []const Expr,
    },
    DictLiteral: struct {
        keys: []const Expr,
        values: []const Expr,
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
    FString: struct {
        parts: []const Expr,
    },
    Grouping: *const Expr,
    Variable: struct {
        name: []const u8,
        depth: i32, // -1 => global
        slot: i32, // -1 => global
    },
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
        slot: i32, // -1 => global
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
        slot: i32, // -1 => global
    },
    Function: struct {
        name: []const u8,
        params: []const []const u8,
        body: []const Stmt,
        locals_count: usize,
        this_slot: i32, // -1 => no "this"
        slot: i32, // -1 => global
    },
    Class: struct {
        name: []const u8,
        methods: []const Stmt,
    },
    Return: struct {
        value: ?Expr,
    },
    Import: struct {
        name: []const u8,
    },
    Raise: struct {
        value: Expr,
    },
    Try: struct {
        try_branch: *const Stmt,
        except_branch: *const Stmt,
    },
};
