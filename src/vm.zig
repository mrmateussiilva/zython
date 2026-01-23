const std = @import("std");
const AST = @import("ast.zig");
const Bytecode = @import("bytecode.zig");

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
    ip: usize,
    base: usize,
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    globals: std.StringHashMap(Value),
    stack: std.ArrayList(Value),
    frames: std.ArrayList(CallFrame),

    pub fn init(allocator: std.mem.Allocator) !VM {
        var globals = std.StringHashMap(Value).init(allocator);
        try globals.put("len", Value{ .NativeFunction = nativeLen });
        try globals.put("open", Value{ .NativeFunction = nativeOpen });
        return .{
            .allocator = allocator,
            .globals = globals,
            .stack = std.ArrayList(Value){},
            .frames = std.ArrayList(CallFrame){},
        };
    }

    pub fn deinit(self: *VM) void {
        self.globals.deinit();
        self.stack.deinit(self.allocator);
        self.frames.deinit(self.allocator);
    }

    pub fn interpret(self: *VM, func: *BytecodeFunction) VMError!void {
        self.stack.items.len = 0;
        try self.frames.append(self.allocator, .{ .func = func, .ip = 0, .base = 0 });

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
                    if (self.globals.get(name.String)) |val| {
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
                    try self.globals.put(name.String, val);
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
                .GreaterEqual => try self.binaryNumberOp(.GreaterEqual),
                .Less => try self.binaryNumberOp(.Less),
                .LessEqual => try self.binaryNumberOp(.LessEqual),
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
                .Subtract => try self.binaryNumberOp(.Subtract),
                .Multiply => try self.binaryNumberOp(.Multiply),
                .Divide => try self.binaryNumberOp(.Divide),
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
                .Return => {
                    const result = self.pop();
                    const finished = self.frames.pop() orelse return VMError.RuntimeError;
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
        switch (callee) {
            .BytecodeFunction => |ptr| {
                const func = @as(*BytecodeFunction, @ptrCast(@alignCast(ptr)));
                if (func.arity != arg_count) return VMError.RuntimeError;
                const base = callee_index + 1;
                if (func.locals_count < arg_count) return VMError.RuntimeError;
                const frame = CallFrame{
                    .func = func,
                    .ip = 0,
                    .base = base,
                };
                try self.frames.append(self.allocator, frame);

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
            else => return VMError.RuntimeError,
        }
    }

    fn binaryNumberOp(self: *VM, op: OpCode) VMError!void {
        const b = self.pop();
        const a = self.pop();
        if (a != .Number or b != .Number) return VMError.TypeError;
        const res = switch (op) {
            .Greater => Value{ .Boolean = a.Number > b.Number },
            .GreaterEqual => Value{ .Boolean = a.Number >= b.Number },
            .Less => Value{ .Boolean = a.Number < b.Number },
            .LessEqual => Value{ .Boolean = a.Number <= b.Number },
            .Subtract => Value{ .Number = a.Number - b.Number },
            .Multiply => Value{ .Number = a.Number * b.Number },
            .Divide => Value{ .Number = a.Number / b.Number },
            else => return VMError.RuntimeError,
        };
        try self.push(res);
    }
};

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
