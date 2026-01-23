const std = @import("std");
const AST = @import("ast.zig");
const Bytecode = @import("bytecode.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Resolver = @import("resolver.zig").Resolver;
const Compiler = @import("compiler.zig").Compiler;

const Value = AST.Value;
const OpCode = Bytecode.OpCode;
const BytecodeFunction = Bytecode.BytecodeFunction;

pub const VMError = error{
    RuntimeError,
    TypeError,
    UndefinedVariable,
    OutOfMemory,
};

const CallFrame = struct {
    func: *BytecodeFunction,
    globals: *std.StringHashMap(Value),
    ip: usize,
    base: usize,
};

const TryFrame = struct {
    frame_index: usize,
    handler_ip: usize,
    stack_len: usize,
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    globals: *std.StringHashMap(Value),
    owns_globals: bool,
    stack: std.ArrayList(Value),
    frames: std.ArrayList(CallFrame),
    try_stack: std.ArrayList(TryFrame),

    pub fn init(allocator: std.mem.Allocator) !VM {
        const globals = try allocator.create(std.StringHashMap(Value));
        globals.* = std.StringHashMap(Value).init(allocator);
        try globals.put("len", Value{ .NativeFunction = nativeLen });
        try globals.put("open", Value{ .NativeFunction = nativeOpen });
        return .{
            .allocator = allocator,
            .globals = globals,
            .owns_globals = true,
            .stack = std.ArrayList(Value){},
            .frames = std.ArrayList(CallFrame){},
            .try_stack = std.ArrayList(TryFrame){},
        };
    }

    pub fn initWithGlobals(allocator: std.mem.Allocator, globals: *std.StringHashMap(Value)) VM {
        return .{
            .allocator = allocator,
            .globals = globals,
            .owns_globals = false,
            .stack = std.ArrayList(Value){},
            .frames = std.ArrayList(CallFrame){},
            .try_stack = std.ArrayList(TryFrame){},
        };
    }

    pub fn deinit(self: *VM) void {
        if (self.owns_globals) {
            self.globals.deinit();
            self.allocator.destroy(self.globals);
        }
        self.stack.deinit(self.allocator);
        self.frames.deinit(self.allocator);
        self.try_stack.deinit(self.allocator);
    }

    pub fn interpret(self: *VM, func: *BytecodeFunction) VMError!void {
        self.stack.items.len = 0;
        if (func.globals == null) func.globals = self.globals;
        try self.frames.append(self.allocator, .{ .func = func, .globals = self.globals, .ip = 0, .base = 0 });

        if (func.locals_count > 0) {
            try self.stack.ensureTotalCapacity(self.allocator, func.locals_count);
            while (self.stack.items.len < func.locals_count) {
                self.stack.appendAssumeCapacity(Value{ .Nil = {} });
            }
        }

        _ = try self.run();
    }

    fn run(self: *VM) VMError!Value {
        while (true) {
            var frame = &self.frames.items[self.frames.items.len - 1];
            const code = frame.func.chunk.code.items;
            if (frame.ip >= code.len) return VMError.RuntimeError;
            const op = @as(OpCode, @enumFromInt(code[frame.ip]));
            frame.ip += 1;

            switch (op) {
                .Constant => {
                    const idx = self.readU16(frame, code);
                    const val = frame.func.chunk.constants.items[idx];
                    try self.push(val);
                },
                .Nil => try self.push(Value{ .Nil = {} }),
                .True => try self.push(Value{ .Boolean = true }),
                .False => try self.push(Value{ .Boolean = false }),
                .Pop => {
                    _ = self.pop();
                },
                .GetLocal => {
                    const slot = self.readU16(frame, code);
                    const val = self.stack.items[frame.base + slot];
                    try self.push(val);
                },
                .SetLocal => {
                    const slot = self.readU16(frame, code);
                    const val = self.peek(0);
                    self.stack.items[frame.base + slot] = val;
                },
                .GetGlobal => {
                    const idx = self.readU16(frame, code);
                    const name = frame.func.chunk.constants.items[idx];
                    if (name != .String) return VMError.RuntimeError;
                    if (frame.globals.get(name.String)) |val| {
                        try self.push(val);
                    } else {
                        return VMError.UndefinedVariable;
                    }
                },
                .DefineGlobal => {
                    const idx = self.readU16(frame, code);
                    const name = frame.func.chunk.constants.items[idx];
                    if (name != .String) return VMError.RuntimeError;
                    const val = self.pop();
                    try frame.globals.put(name.String, val);
                    if (val == .BytecodeFunction) {
                        const func = @as(*BytecodeFunction, @ptrCast(@alignCast(val.BytecodeFunction)));
                        func.globals = frame.globals;
                    }
                },
                .Equal => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.push(Value{ .Boolean = isEqual(a, b) });
                },
                .NotEqual => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.push(Value{ .Boolean = !isEqual(a, b) });
                },
                .Greater => try self.binaryNumberOp(.Greater),
                .GreaterNum => try self.binaryNumberOp(.GreaterNum),
                .GreaterEqual => try self.binaryNumberOp(.GreaterEqual),
                .GreaterEqualNum => try self.binaryNumberOp(.GreaterEqualNum),
                .Less => try self.binaryNumberOp(.Less),
                .LessNum => try self.binaryNumberOp(.LessNum),
                .LessEqual => try self.binaryNumberOp(.LessEqual),
                .LessEqualNum => try self.binaryNumberOp(.LessEqualNum),
                .Add => {
                    const b = self.pop();
                    const a = self.pop();
                    if (a == .Number and b == .Number) {
                        try self.push(Value{ .Number = a.Number + b.Number });
                    } else if (a == .String and b == .String) {
                        const res = try self.allocator.alloc(u8, a.String.len + b.String.len);
                        std.mem.copyForwards(u8, res[0..a.String.len], a.String);
                        std.mem.copyForwards(u8, res[a.String.len..], b.String);
                        try self.push(Value{ .String = res });
                    } else {
                        return VMError.TypeError;
                    }
                },
                .AddNum => try self.binaryNumberOp(.AddNum),
                .Subtract => try self.binaryNumberOp(.Subtract),
                .SubtractNum => try self.binaryNumberOp(.SubtractNum),
                .Multiply => try self.binaryNumberOp(.Multiply),
                .MultiplyNum => try self.binaryNumberOp(.MultiplyNum),
                .Divide => try self.binaryNumberOp(.Divide),
                .DivideNum => try self.binaryNumberOp(.DivideNum),
                .Not => {
                    const v = self.pop();
                    try self.push(Value{ .Boolean = !isTruthy(v) });
                },
                .Negate => {
                    const v = self.pop();
                    if (v != .Number) return VMError.TypeError;
                    try self.push(Value{ .Number = -v.Number });
                },
                .Print => {
                    const val = self.pop();
                    const str = val.toString(self.allocator) catch return VMError.OutOfMemory;
                    std.debug.print("{s}\n", .{str});
                },
                .Jump => {
                    const offset = self.readU16(frame, code);
                    frame.ip += offset;
                },
                .JumpIfFalse => {
                    const offset = self.readU16(frame, code);
                    const cond = self.peek(0);
                    if (!isTruthy(cond)) {
                        frame.ip += offset;
                    }
                },
                .Loop => {
                    const offset = self.readU16(frame, code);
                    frame.ip -= offset;
                },
                .Call => {
                    const arg_count = self.readU8(frame, code);
                    try self.callValue(arg_count);
                },
                .TryBegin => {
                    const offset = self.readU16(frame, code);
                    const handler_ip = frame.ip + offset;
                    const tf = TryFrame{
                        .frame_index = self.frames.items.len - 1,
                        .handler_ip = handler_ip,
                        .stack_len = self.stack.items.len,
                    };
                    try self.try_stack.append(self.allocator, tf);
                },
                .TryEnd => {
                    _ = self.try_stack.pop() orelse return VMError.RuntimeError;
                },
                .Raise => {
                    _ = self.pop();
                    if (self.try_stack.items.len == 0) return VMError.RuntimeError;

                    const tf = self.try_stack.pop() orelse return VMError.RuntimeError;
                    while (self.frames.items.len - 1 > tf.frame_index) {
                        _ = self.frames.pop() orelse return VMError.RuntimeError;
                    }
                    if (self.frames.items.len == 0) return VMError.RuntimeError;

                    self.stack.items.len = tf.stack_len;
                    frame = &self.frames.items[self.frames.items.len - 1];
                    frame.ip = tf.handler_ip;
                },
                .GetAttr => {
                    const idx = self.readU16(frame, code);
                    const name = frame.func.chunk.constants.items[idx];
                    if (name != .String) return VMError.RuntimeError;
                    const obj = self.pop();
                    switch (obj) {
                        .Module => |m| {
                            const module_globals = @as(*std.StringHashMap(Value), @ptrCast(@alignCast(m.exports)));
                            if (module_globals.get(name.String)) |val| {
                                try self.push(val);
                            } else {
                                return VMError.UndefinedVariable;
                            }
                        },
                        .List => |list| {
                            if (!std.mem.eql(u8, name.String, "append")) return VMError.RuntimeError;
                            const fn_val = Value{ .Function = .{
                                .name = "__list_append__",
                                .params = @as([]const []const u8, &[_][]const u8{"item"}),
                                .body = &[_]AST.Stmt{},
                                .closure = @as(*anyopaque, @ptrCast(list)),
                                .locals_count = 0,
                                .this_slot = -1,
                            }};
                            try self.push(fn_val);
                        },
                        .String => |s| {
                            if (std.mem.eql(u8, name.String, "strip")) {
                                const ctx = try self.allocator.create([]const u8);
                                ctx.* = s;
                                const fn_val = Value{ .Function = .{
                                    .name = "__str_strip__",
                                    .params = &[_][]const u8{},
                                    .body = &[_]AST.Stmt{},
                                    .closure = @as(*anyopaque, @ptrCast(ctx)),
                                    .locals_count = 0,
                                    .this_slot = -1,
                                }};
                                try self.push(fn_val);
                                break;
                            }
                            if (std.mem.eql(u8, name.String, "split")) {
                                const ctx = try self.allocator.create([]const u8);
                                ctx.* = s;
                                const fn_val = Value{ .Function = .{
                                    .name = "__str_split__",
                                    .params = @as([]const []const u8, &[_][]const u8{"delimiter"}),
                                    .body = &[_]AST.Stmt{},
                                    .closure = @as(*anyopaque, @ptrCast(ctx)),
                                    .locals_count = 0,
                                    .this_slot = -1,
                                }};
                                try self.push(fn_val);
                                break;
                            }
                            return VMError.RuntimeError;
                        },
                        .File => |f| {
                            const ctx = try self.allocator.create(std.fs.File);
                            ctx.* = f;
                            if (std.mem.eql(u8, name.String, "read")) {
                                const fn_val = Value{ .Function = .{
                                    .name = "__file_read__",
                                    .params = &[_][]const u8{},
                                    .body = &[_]AST.Stmt{},
                                    .closure = @as(*anyopaque, @ptrCast(ctx)),
                                    .locals_count = 0,
                                    .this_slot = -1,
                                }};
                                try self.push(fn_val);
                                break;
                            }
                            if (std.mem.eql(u8, name.String, "write")) {
                                const fn_val = Value{ .Function = .{
                                    .name = "__file_write__",
                                    .params = @as([]const []const u8, &[_][]const u8{"data"}),
                                    .body = &[_]AST.Stmt{},
                                    .closure = @as(*anyopaque, @ptrCast(ctx)),
                                    .locals_count = 0,
                                    .this_slot = -1,
                                }};
                                try self.push(fn_val);
                                break;
                            }
                            if (std.mem.eql(u8, name.String, "close")) {
                                const fn_val = Value{ .Function = .{
                                    .name = "__file_close__",
                                    .params = &[_][]const u8{},
                                    .body = &[_]AST.Stmt{},
                                    .closure = @as(*anyopaque, @ptrCast(ctx)),
                                    .locals_count = 0,
                                    .this_slot = -1,
                                }};
                                try self.push(fn_val);
                                break;
                            }
                            return VMError.RuntimeError;
                        },
                        else => return VMError.RuntimeError,
                    }
                },
                .Import => {
                    const idx = self.readU16(frame, code);
                    const name = frame.func.chunk.constants.items[idx];
                    if (name != .String) return VMError.RuntimeError;
                    const module = try self.loadModule(name.String);
                    try self.push(module);
                },
                .Return => {
                    const result = self.pop();
                    const finished = self.frames.pop() orelse return VMError.RuntimeError;
                    while (self.try_stack.items.len > 0) {
                        const top = self.try_stack.items[self.try_stack.items.len - 1];
                        if (top.frame_index != self.frames.items.len) break;
                        _ = self.try_stack.pop() orelse break;
                    }
                    if (self.frames.items.len == 0) {
                        return result;
                    }
                    if (finished.base == 0) return VMError.RuntimeError;
                    self.stack.items.len = finished.base - 1;
                    try self.push(result);
                },
                .BuildList => {
                    const count = self.readU16(frame, code);
                    const start = self.stack.items.len - count;
                    var elements = std.ArrayList(Value){};
                    try elements.ensureTotalCapacity(self.allocator, count);
                    for (self.stack.items[start..]) |v| {
                        elements.appendAssumeCapacity(v);
                    }
                    self.stack.items.len = start;

                    const list = try self.allocator.create(AST.LoxList);
                    list.* = AST.LoxList{ .elements = elements };
                    try self.push(Value{ .List = list });
                },
                .BuildDict => {
                    const count = self.readU16(frame, code);
                    if (self.stack.items.len < count * 2) return VMError.RuntimeError;
                    const start = self.stack.items.len - (count * 2);
                    const dict = try self.allocator.create(AST.LoxDict);
                    dict.* = AST.LoxDict.init(self.allocator);

                    var i: usize = 0;
                    while (i < count) : (i += 1) {
                        const key = self.stack.items[start + (i * 2)];
                        const value = self.stack.items[start + (i * 2) + 1];
                        try dict.put(key, value);
                    }
                    self.stack.items.len = start;
                    try self.push(Value{ .Dict = dict });
                },
                .GetSubscript => {
                    const index = self.pop();
                    const obj = self.pop();
                    switch (obj) {
                        .List => |list| {
                            if (index != .Number) return VMError.TypeError;
                            const idx_f = index.Number;
                            if (idx_f < 0 or idx_f >= @as(f64, @floatFromInt(list.elements.items.len))) {
                                return VMError.RuntimeError;
                            }
                            const idx = @as(usize, @intFromFloat(idx_f));
                            try self.push(list.elements.items[idx]);
                        },
                        .Dict => |dict| {
                            if (dict.get(index)) |val| {
                                try self.push(val);
                            } else {
                                return VMError.RuntimeError;
                            }
                        },
                        else => return VMError.TypeError,
                    }
                },
                .SetSubscript => {
                    const value = self.pop();
                    const index = self.pop();
                    const obj = self.pop();
                    switch (obj) {
                        .List => |list| {
                            if (index != .Number) return VMError.TypeError;
                            const idx_f = index.Number;
                            if (idx_f < 0 or idx_f >= @as(f64, @floatFromInt(list.elements.items.len))) {
                                return VMError.RuntimeError;
                            }
                            const idx = @as(usize, @intFromFloat(idx_f));
                            list.elements.items[idx] = value;
                            try self.push(value);
                        },
                        .Dict => |dict| {
                            try dict.put(index, value);
                            try self.push(value);
                        },
                        else => return VMError.TypeError,
                    }
                },
                .ListLen => {
                    const obj = self.pop();
                    if (obj != .List) return VMError.TypeError;
                    try self.push(Value{ .Number = @as(f64, @floatFromInt(obj.List.elements.items.len)) });
                },
            }
        }
        return Value{ .Nil = {} };
    }

    fn readU8(self: *VM, frame: *CallFrame, code: []const u8) u8 {
        _ = self;
        const b = code[frame.ip];
        frame.ip += 1;
        return b;
    }

    fn readU16(self: *VM, frame: *CallFrame, code: []const u8) u16 {
        _ = self;
        const hi = code[frame.ip];
        const lo = code[frame.ip + 1];
        frame.ip += 2;
        return (@as(u16, hi) << 8) | @as(u16, lo);
    }

    fn push(self: *VM, value: Value) VMError!void {
        try self.stack.append(self.allocator, value);
    }

    fn pop(self: *VM) Value {
        return self.stack.pop() orelse unreachable;
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    fn callValue(self: *VM, arg_count: u8) VMError!void {
        const callee_index = self.stack.items.len - 1 - arg_count;
        const callee = self.stack.items[callee_index];
        const current_frame = &self.frames.items[self.frames.items.len - 1];
        switch (callee) {
            .BytecodeFunction => |ptr| {
                const func = @as(*BytecodeFunction, @ptrCast(@alignCast(ptr)));
                if (func.arity != arg_count) return VMError.RuntimeError;
                const base = callee_index + 1;
                if (func.locals_count < arg_count) return VMError.RuntimeError;
                const globals = func.globals orelse current_frame.globals;
                const call_frame = CallFrame{
                    .func = func,
                    .globals = globals,
                    .ip = 0,
                    .base = base,
                };
                try self.frames.append(self.allocator, call_frame);

                const needed = base + func.locals_count;
                if (self.stack.items.len < needed) {
                    try self.stack.ensureTotalCapacity(self.allocator, needed);
                    while (self.stack.items.len < needed) {
                        self.stack.appendAssumeCapacity(Value{ .Nil = {} });
                    }
                }
            },
            .NativeFunction => |native| {
                const args_slice = self.stack.items[callee_index + 1 ..];
                const result = native(self.allocator, args_slice) catch return VMError.RuntimeError;
                self.stack.items.len = callee_index;
                try self.push(result);
            },
            .Function => |func| {
                if (std.mem.eql(u8, func.name, "__list_append__")) {
                    if (arg_count != 1) return VMError.RuntimeError;
                    const list = @as(*AST.LoxList, @ptrCast(@alignCast(func.closure.?)));
                    const value = self.stack.items[callee_index + 1];
                    try list.elements.append(self.allocator, value);
                    self.stack.items.len = callee_index;
                    try self.push(Value{ .Nil = {} });
                    return;
                }
                if (std.mem.eql(u8, func.name, "__str_strip__")) {
                    if (arg_count != 0) return VMError.RuntimeError;
                    const str_ptr = @as(*[]const u8, @ptrCast(@alignCast(func.closure.?)));
                    const trimmed = std.mem.trim(u8, str_ptr.*, " \t\r\n");
                    const duped = try self.allocator.dupe(u8, trimmed);
                    self.stack.items.len = callee_index;
                    try self.push(Value{ .String = duped });
                    return;
                }
                if (std.mem.eql(u8, func.name, "__str_split__")) {
                    if (arg_count != 1) return VMError.RuntimeError;
                    const str_ptr = @as(*[]const u8, @ptrCast(@alignCast(func.closure.?)));
                    const delim = self.stack.items[callee_index + 1];
                    if (delim != .String) return VMError.TypeError;

                    var parts = std.ArrayList(Value){};
                    var iter = std.mem.splitSequence(u8, str_ptr.*, delim.String);
                    while (iter.next()) |part| {
                        try parts.append(self.allocator, Value{ .String = try self.allocator.dupe(u8, part) });
                    }

                    const list = try self.allocator.create(AST.LoxList);
                    list.* = AST.LoxList{ .elements = parts };
                    self.stack.items.len = callee_index;
                    try self.push(Value{ .List = list });
                    return;
                }
                if (std.mem.eql(u8, func.name, "__file_read__")) {
                    if (arg_count != 0) return VMError.RuntimeError;
                    const file_ptr = @as(*std.fs.File, @ptrCast(@alignCast(func.closure.?)));
                    const content = file_ptr.readToEndAlloc(self.allocator, 1024 * 1024 * 10) catch return VMError.RuntimeError;
                    self.stack.items.len = callee_index;
                    try self.push(Value{ .String = content });
                    return;
                }
                if (std.mem.eql(u8, func.name, "__file_write__")) {
                    if (arg_count != 1) return VMError.RuntimeError;
                    const val = self.stack.items[callee_index + 1];
                    if (val != .String) return VMError.TypeError;
                    const file_ptr = @as(*std.fs.File, @ptrCast(@alignCast(func.closure.?)));
                    file_ptr.writeAll(val.String) catch return VMError.RuntimeError;
                    self.stack.items.len = callee_index;
                    try self.push(Value{ .Nil = {} });
                    return;
                }
                if (std.mem.eql(u8, func.name, "__file_close__")) {
                    if (arg_count != 0) return VMError.RuntimeError;
                    const file_ptr = @as(*std.fs.File, @ptrCast(@alignCast(func.closure.?)));
                    file_ptr.close();
                    self.stack.items.len = callee_index;
                    try self.push(Value{ .Nil = {} });
                    return;
                }
                return VMError.RuntimeError;
            },
            else => return VMError.RuntimeError,
        }
    }

    fn binaryNumberOp(self: *VM, op: OpCode) VMError!void {
        const b = self.pop();
        const a = self.pop();
        if (a != .Number or b != .Number) return VMError.TypeError;
        const res = switch (op) {
            .Greater => Value{ .Boolean = a.Number > b.Number },
            .GreaterNum => Value{ .Boolean = a.Number > b.Number },
            .GreaterEqual => Value{ .Boolean = a.Number >= b.Number },
            .GreaterEqualNum => Value{ .Boolean = a.Number >= b.Number },
            .Less => Value{ .Boolean = a.Number < b.Number },
            .LessNum => Value{ .Boolean = a.Number < b.Number },
            .LessEqual => Value{ .Boolean = a.Number <= b.Number },
            .LessEqualNum => Value{ .Boolean = a.Number <= b.Number },
            .AddNum => Value{ .Number = a.Number + b.Number },
            .Subtract => Value{ .Number = a.Number - b.Number },
            .SubtractNum => Value{ .Number = a.Number - b.Number },
            .Multiply => Value{ .Number = a.Number * b.Number },
            .MultiplyNum => Value{ .Number = a.Number * b.Number },
            .Divide => Value{ .Number = a.Number / b.Number },
            .DivideNum => Value{ .Number = a.Number / b.Number },
            else => return VMError.RuntimeError,
        };
        try self.push(res);
    }

    fn loadModule(self: *VM, name: []const u8) VMError!Value {
        const source = try loadSource(self.allocator, name);
        defer self.allocator.free(source);

        var lexer = Lexer.init(self.allocator, source);
        defer lexer.deinit();

        var parser = Parser.init(self.allocator, &lexer) catch return VMError.RuntimeError;
        var statements = parser.parse() catch return VMError.RuntimeError;
        defer statements.deinit(self.allocator);

        var resolver = Resolver.init(self.allocator);
        defer resolver.deinit();
        resolver.resolve(statements.items) catch return VMError.RuntimeError;

        var compiler = Compiler.init(self.allocator, 0);
        const bc_func = compiler.compileScript(statements.items) catch return VMError.RuntimeError;

        const module_globals = try self.allocator.create(std.StringHashMap(Value));
        module_globals.* = std.StringHashMap(Value).init(self.allocator);
        try copyBuiltins(module_globals, self.globals);

        var module_vm = VM.initWithGlobals(self.allocator, module_globals);
        defer module_vm.deinit();
        module_vm.interpret(bc_func) catch return VMError.RuntimeError;

        return Value{ .Module = .{ .name = try self.allocator.dupe(u8, name), .exports = module_globals } };
    }
};

fn loadSource(allocator: std.mem.Allocator, name: []const u8) VMError![]u8 {
    const file_name = std.fmt.allocPrint(allocator, "{s}.py", .{name}) catch return VMError.OutOfMemory;
    defer allocator.free(file_name);

    const file = std.fs.cwd().openFile(file_name, .{}) catch return VMError.RuntimeError;
    defer file.close();
    const source = file.readToEndAlloc(allocator, 1024 * 1024) catch return VMError.RuntimeError;
    return source;
}

fn copyBuiltins(dst: *std.StringHashMap(Value), src: *std.StringHashMap(Value)) VMError!void {
    var it = src.iterator();
    while (it.next()) |entry| {
        try dst.put(entry.key_ptr.*, entry.value_ptr.*);
    }
}

fn isTruthy(value: Value) bool {
    return switch (value) {
        .Nil => false,
        .Boolean => |b| b,
        .Number => |n| n != 0,
        .String => |s| s.len > 0,
        .Function => true,
        .BytecodeFunction => true,
        .NativeFunction => true,
        .Class => true,
        .Instance => true,
        .List => |l| l.elements.items.len > 0,
        .File => true,
        .Dict => |d| d.count() > 0,
        .Module => true,
    };
}

fn isEqual(a: Value, b: Value) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .Nil => true,
        .Boolean => a.Boolean == b.Boolean,
        .Number => a.Number == b.Number,
        .String => std.mem.eql(u8, a.String, b.String),
        .Function => false,
        .BytecodeFunction => a.BytecodeFunction == b.BytecodeFunction,
        .NativeFunction => false,
        .Class => a.Class == b.Class,
        .Instance => a.Instance == b.Instance,
        .List => a.List == b.List,
        .File => a.File.handle == b.File.handle,
        .Dict => a.Dict == b.Dict,
        .Module => a.Module.exports == b.Module.exports,
    };
}

fn nativeLen(allocator: std.mem.Allocator, args: []const Value) VMError!Value {
    _ = allocator;
    if (args.len != 1) return VMError.RuntimeError;
    switch (args[0]) {
        .String => |s| return Value{ .Number = @as(f64, @floatFromInt(s.len)) },
        .List => |l| return Value{ .Number = @as(f64, @floatFromInt(l.elements.items.len)) },
        .Dict => |d| return Value{ .Number = @as(f64, @floatFromInt(d.count())) },
        else => return VMError.TypeError,
    }
}

fn nativeOpen(allocator: std.mem.Allocator, args: []const Value) VMError!Value {
    _ = allocator;
    if (args.len != 2) return VMError.RuntimeError;
    if (args[0] != .String or args[1] != .String) return VMError.TypeError;

    const path = args[0].String;
    const mode = args[1].String;

    if (std.mem.eql(u8, mode, "r")) {
        const file = std.fs.cwd().openFile(path, .{}) catch return VMError.RuntimeError;
        return Value{ .File = file };
    } else if (std.mem.eql(u8, mode, "w")) {
        const file = std.fs.cwd().createFile(path, .{}) catch return VMError.RuntimeError;
        return Value{ .File = file };
    }
    return VMError.RuntimeError;
}
