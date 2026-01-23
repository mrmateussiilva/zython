const std = @import("std");
const AST = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Expr = AST.Expr;
const Stmt = AST.Stmt;
const Value = AST.Value;
const BinaryOp = AST.BinaryOp;

pub const InterpreterError = AST.InterpreterError;

const Environment = struct {
    values: std.StringHashMap(Value),
    enclosing: ?*Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment) !*Environment {
        const env = try allocator.create(Environment);
        env.* = Environment{
            .values = std.StringHashMap(Value).init(allocator),
            .enclosing = enclosing,
            .allocator = allocator,
        };
        return env;
    }

    pub fn deinit(self: *Environment) void {
        self.values.deinit();
        self.allocator.destroy(self);
    }

    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        try self.values.put(name, value);
    }

    pub fn assign(self: *Environment, name: []const u8, value: Value) !void {
        if (self.values.contains(name)) {
            try self.values.put(name, value);
            return;
        }
        if (self.enclosing) |enc| {
            try enc.assign(name, value);
            return;
        }
        return InterpreterError.UndefinedVariable;
    }

    pub fn get(self: *Environment, name: []const u8) !Value {
        if (self.values.get(name)) |val| {
            return val;
        }
        if (self.enclosing) |enc| {
            return enc.get(name);
        }
        return InterpreterError.UndefinedVariable;
    }
};

fn nativeLen(allocator: std.mem.Allocator, args: []const Value) InterpreterError!Value {
    _ = allocator;
    if (args.len != 1) {
        std.debug.print("len() takes exactly one argument.\n", .{});
        return InterpreterError.RuntimeError;
    }
    switch (args[0]) {
        .String => |s| return Value{ .Number = @as(f64, @floatFromInt(s.len)) },
        .List => |l| return Value{ .Number = @as(f64, @floatFromInt(l.elements.items.len)) },
        .Dict => |d| return Value{ .Number = @as(f64, @floatFromInt(d.count())) },
        else => {
            std.debug.print("Object of type 'number' has no len().\n", .{}); // Generic error message mimicking Python
            return InterpreterError.TypeError;
        }
    }
}

fn nativeOpen(allocator: std.mem.Allocator, args: []const Value) InterpreterError!Value {
    _ = allocator;
    if (args.len != 2) return InterpreterError.RuntimeError;
    if (args[0] != .String or args[1] != .String) return InterpreterError.TypeError;

    const path = args[0].String;
    const mode = args[1].String;

    if (std.mem.eql(u8, mode, "r")) {
        const file = std.fs.cwd().openFile(path, .{}) catch return InterpreterError.RuntimeError;
        return Value{ .File = file };
    } else if (std.mem.eql(u8, mode, "w")) {
        const file = std.fs.cwd().createFile(path, .{}) catch return InterpreterError.RuntimeError;
        return Value{ .File = file };
    }
    return InterpreterError.RuntimeError;
}

pub const Interpreter = struct {
    globals: *Environment,
    environment: *Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Interpreter {
        const globals = try Environment.init(allocator, null);
        try globals.define("len", Value{ .NativeFunction = nativeLen });
        try globals.define("open", Value{ .NativeFunction = nativeOpen });
        return Interpreter{
            .globals = globals,
            .environment = globals,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.globals.deinit();
    }

    fn loadModule(self: *Interpreter, name: []const u8) !Value {
        const fileName = try std.fmt.allocPrint(self.allocator, "{s}.py", .{name});
        defer self.allocator.free(fileName);
        
        const file = std.fs.cwd().openFile(fileName, .{}) catch |err| {
            std.debug.print("Could not open module '{s}': {any}\n", .{fileName, err});
            return InterpreterError.RuntimeError;
        };
        defer file.close();
        
        const source = try file.readToEndAlloc(self.allocator, 1024 * 1024);
        
        var lexer = Lexer.init(self.allocator, source);
        defer lexer.deinit(); 
        
        var parser = try Parser.init(self.allocator, &lexer);
        
        var statements = try parser.parse();
        defer statements.deinit(self.allocator);

        const moduleEnv = try Environment.init(self.allocator, self.globals); 
        
        const previousEnv = self.environment;
        self.environment = moduleEnv;
        
        for (statements.items) |stmt| {
            _ = try self.execute(stmt);
        }
        
        self.environment = previousEnv;
        
        return Value{ .Module = .{ .name = try self.allocator.dupe(u8, name), .exports = moduleEnv } };
    }

    pub fn interpret(self: *Interpreter, statements: []const Stmt) !void {
        for (statements) |stmt| {
            _ = try self.execute(stmt);
        }
    }

    fn execute(self: *Interpreter, stmt: Stmt) InterpreterError!?Value {
        switch (stmt) {
            .Print => |s| {
                const val = try self.evaluate(s.expression);
                const str = val.toString(self.allocator) catch return InterpreterError.OutOfMemory;
                std.debug.print("{s}\n", .{str});
                return null;
            },
            .Expression => |s| {
                _ = try self.evaluate(s.expression);
                return null;
            },
            .Var => |s| {
                const val = try self.evaluate(s.initializer);
                try self.environment.define(s.name, val);
                return null;
            },
            .Block => |b| {
                const env = Environment.init(self.allocator, self.environment) catch return InterpreterError.OutOfMemory;
                return try self.executeBlock(b.statements, env);
            },
            .If => |s| {
                const condition = try self.evaluate(s.condition);
                if (self.isTruthy(condition)) {
                    if (try self.execute(s.then_branch.*)) |ret| return ret;
                } else if (s.else_branch) |else_branch| {
                    if (try self.execute(else_branch.*)) |ret| return ret;
                }
                return null;
            },
            .While => |s| {
                while (self.isTruthy(try self.evaluate(s.condition))) {
                    if (try self.execute(s.body.*)) |ret| return ret;
                }
                return null;
            },
            .For => |f| {
                const iterable = try self.evaluate(f.iterable);
                if (iterable != .List) return InterpreterError.TypeError;

                const list = iterable.List;
                for (list.elements.items) |item| {
                    try self.environment.define(f.variable, item);
                    if (try self.execute(f.body.*)) |ret| return ret;
                }
                return null;
            },
            .Function => |s| {
                const func = Value{ .Function = .{ 
                    .name = s.name,
                    .params = s.params,
                    .body = s.body,
                    .closure = self.environment,
                }};
                try self.environment.define(s.name, func);
                return null;
            },
            .Class => |c| {
                const klass = try self.allocator.create(AST.LoxClass);
                klass.* = AST.LoxClass{
                    .name = c.name,
                    .methods = std.StringHashMap(Value).init(self.allocator),
                };

                try self.environment.define(c.name, Value{ .Class = klass });

                for (c.methods) |methodStmt| {
                     switch (methodStmt) {
                         .Function => |f| {
                             const funcVal = Value{ .Function = .{ 
                                 .name = f.name,
                                 .params = f.params,
                                 .body = f.body,
                                 .closure = self.environment,
                             }};
                             try klass.methods.put(f.name, funcVal);
                         },
                         else => unreachable,
                     }
                }
                return null;
            },
            .Return => |r| {
                const val = if (r.value) |v| try self.evaluate(v) else Value{ .Nil = {} };
                return val;
            },
            .Import => |imp| {
                const module = self.loadModule(imp.name) catch |err| {
                    std.debug.print("Error importing module '{s}': {any}\n", .{imp.name, err});
                    return InterpreterError.RuntimeError;
                };
                try self.environment.define(imp.name, module);
                return null;
            },
        }
    }

    fn executeBlock(self: *Interpreter, statements: []const Stmt, env: *Environment) InterpreterError!?Value {
        const previous = self.environment;
        self.environment = env;
        defer {
            self.environment = previous;
        }

        for (statements) |stmt| {
            if (try self.execute(stmt)) |val| {
                return val;
            }
        }
        return null;
    }

    fn evaluate(self: *Interpreter, expr: Expr) InterpreterError!Value {
        switch (expr) {
            .Literal => |v| return v,
            .Grouping => |g| return self.evaluate(g.*),
            .Unary => |u| {
                const right = try self.evaluate(u.right.*);
                switch (u.op) {
                    .Negate => {
                        if (right == .Number) return Value{ .Number = -right.Number };
                        return InterpreterError.TypeError;
                    },
                    .Not => {
                         return Value{ .Boolean = !self.isTruthy(right) };
                    }
                }
            },
            .Binary => |b| {
                const left = try self.evaluate(b.left.*);
                const right = try self.evaluate(b.right.*);

                switch (b.op) {
                    .Add => {
                        if (left == .Number and right == .Number) return Value{ .Number = left.Number + right.Number };
                        if (left == .String and right == .String) {
                            const res = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{left.String, right.String});
                            return Value{ .String = res };
                        }
                        return InterpreterError.TypeError;
                    },
                    .Sub => {
                        if (left == .Number and right == .Number) return Value{ .Number = left.Number - right.Number };
                        return InterpreterError.TypeError;
                    },
                    .Mul => {
                        if (left == .Number and right == .Number) return Value{ .Number = left.Number * right.Number };
                        return InterpreterError.TypeError;
                    },
                    .Div => {
                        if (left == .Number and right == .Number) return Value{ .Number = left.Number / right.Number };
                        return InterpreterError.TypeError;
                    },
                    .Equal => {
                        return Value{ .Boolean = self.isEqual(left, right) };
                    },
                    .NotEqual => {
                         return Value{ .Boolean = !self.isEqual(left, right) };
                    },
                    .Greater => {
                        if (left == .Number and right == .Number) return Value{ .Boolean = left.Number > right.Number };
                        return InterpreterError.TypeError;
                    },
                    .GreaterEqual => {
                        if (left == .Number and right == .Number) return Value{ .Boolean = left.Number >= right.Number };
                        return InterpreterError.TypeError;
                    },
                    .Less => {
                        if (left == .Number and right == .Number) return Value{ .Boolean = left.Number < right.Number };
                        return InterpreterError.TypeError;
                    },
                    .LessEqual => {
                        if (left == .Number and right == .Number) return Value{ .Boolean = left.Number <= right.Number };
                        return InterpreterError.TypeError;
                    },
                }
            },
            .Logical => |l| {
                const left = try self.evaluate(l.left.*);
                if (l.op == .Or) {
                    if (self.isTruthy(left)) return left;
                } else {
                    if (!self.isTruthy(left)) return left;
                }
                return self.evaluate(l.right.*);
            },
            .Call => |c| {
                const callee = try self.evaluate(c.callee.*);
                const args = c.arguments;
                return self.callValue(callee, args);
            },
            .Get => |g| {
                const obj = try self.evaluate(g.object.*);
                switch (obj) {
                    .Instance => |inst| {
                        if (inst.fields.get(g.name)) |val| return val;
                        if (inst.klass.methods.get(g.name)) |method| {
                             return self.bindMethod(inst, method);
                        }
                        return InterpreterError.RuntimeError;
                    },
                    .String => |s| {
                        if (std.mem.eql(u8, g.name, "strip")) {
                            const ctx = try self.allocator.create([]const u8);
                            ctx.* = s;
                            return Value{ .Function = .{
                                .name = "__str_strip__",
                                .params = &[_][]const u8{},
                                .body = &[_]Stmt{},
                                .closure = @as(*anyopaque, @ptrCast(ctx)),
                            }};
                        }
                        if (std.mem.eql(u8, g.name, "split")) {
                            const ctx = try self.allocator.create([]const u8);
                            ctx.* = s;
                            return Value{ .Function = .{
                                .name = "__str_split__",
                                .params = @as([]const []const u8, &[_][]const u8{"delimiter"}),
                                .body = &[_]Stmt{},
                                .closure = @as(*anyopaque, @ptrCast(ctx)),
                            }};
                        }
                        return InterpreterError.RuntimeError;
                    },
                    .List => |list| {
                        if (std.mem.eql(u8, g.name, "append")) {
                            // Hack: Return a special function for append
                            // We treat the 'closure' pointer as the List pointer
                            return Value{ .Function = .{
                                .name = "__list_append__",
                                .params = @as([]const []const u8, &[_][]const u8{"item"}), // Dummy param
                                .body = &[_]Stmt{}, // Empty body
                                .closure = @as(*anyopaque, @ptrCast(list)),
                            }};
                        }
                        return InterpreterError.RuntimeError;
                    },
                    .File => |f| {
                        const ctx = try self.allocator.create(std.fs.File);
                        ctx.* = f;
                        
                        if (std.mem.eql(u8, g.name, "read")) {
                             return Value{ .Function = .{
                                .name = "__file_read__",
                                .params = &[_][]const u8{}, 
                                .body = &[_]Stmt{},
                                .closure = @as(*anyopaque, @ptrCast(ctx)),
                            }};
                        }
                        if (std.mem.eql(u8, g.name, "write")) {
                             return Value{ .Function = .{
                                .name = "__file_write__",
                                .params = @as([]const []const u8, &[_][]const u8{"data"}),
                                .body = &[_]Stmt{},
                                .closure = @as(*anyopaque, @ptrCast(ctx)),
                            }};
                        }
                         if (std.mem.eql(u8, g.name, "close")) {
                             return Value{ .Function = .{
                                .name = "__file_close__",
                                .params = &[_][]const u8{},
                                .body = &[_]Stmt{},
                                .closure = @as(*anyopaque, @ptrCast(ctx)),
                            }};
                        }
                        return InterpreterError.RuntimeError;
                    },
                    .Module => |mod| {
                        const env = @as(*Environment, @ptrCast(@alignCast(mod.exports)));
                        return env.get(g.name);
                    },
                    else => return InterpreterError.TypeError,
                }
            },
            .Set => |s| {
                const obj = try self.evaluate(s.object.*);
                switch (obj) {
                    .Instance => |inst| {
                        const val = try self.evaluate(s.value.*);
                        try inst.fields.put(s.name, val);
                        return val;
                    },
                    else => return InterpreterError.TypeError,
                }
            },
            .This => |_| {
                return self.environment.get("this");
            },
            .Variable => |name| {
                return self.environment.get(name);
            },
            .ListLiteral => |l| {
                var elements = std.ArrayList(Value){};
                for (l.elements) |elemExpr| {
                    const val = try self.evaluate(elemExpr);
                    try elements.append(self.allocator, val);
                }
                const list = try self.allocator.create(AST.LoxList);
                list.* = AST.LoxList{ .elements = elements };
                return Value{ .List = list };
            },
            .DictLiteral => |d| {
                const map = try self.allocator.create(AST.LoxDict);
                map.* = AST.LoxDict.init(self.allocator);
                for (d.keys, 0..) |kExpr, i| {
                    const key = try self.evaluate(kExpr);
                    const val = try self.evaluate(d.values[i]);
                    try map.put(key, val);
                }
                return Value{ .Dict = map };
            },
            .Subscript => |sub| {
                const obj = try self.evaluate(sub.value.*);
                const indexVal = try self.evaluate(sub.index.*);
                
                switch (obj) {
                    .List => |list| {
                        if (indexVal != .Number) return InterpreterError.TypeError;
                        const idx_f = indexVal.Number;
                        if (idx_f < 0 or idx_f >= @as(f64, @floatFromInt(list.elements.items.len))) {
                             std.debug.print("Index out of bounds.\n", .{});
                             return InterpreterError.RuntimeError;
                        }
                        const idx = @as(usize, @intFromFloat(idx_f));
                        return list.elements.items[idx];
                    },
                    .Dict => |dict| {
                        if (dict.get(indexVal)) |val| {
                            return val;
                        }
                        // KeyError handling - simplified
                        return InterpreterError.RuntimeError;
                    },
                    else => return InterpreterError.TypeError,
                }
            },
            .SetSubscript => |setSub| {
                const obj = try self.evaluate(setSub.object.*);
                const indexVal = try self.evaluate(setSub.index.*);
                const val = try self.evaluate(setSub.value.*);

                switch (obj) {
                    .List => |list| {
                        if (indexVal != .Number) return InterpreterError.TypeError;
                        const idx_f = indexVal.Number;
                        if (idx_f < 0 or idx_f >= @as(f64, @floatFromInt(list.elements.items.len))) {
                             std.debug.print("Index out of bounds.\n", .{});
                             return InterpreterError.RuntimeError;
                        }

                        const idx = @as(usize, @intFromFloat(idx_f));
                        list.elements.items[idx] = val;
                        return val;
                    },
                    .Dict => |dict| {
                        try dict.put(indexVal, val);
                        return val;
                    },
                    else => return InterpreterError.TypeError,
                }
            }
        }
    }

    fn callValue(self: *Interpreter, callee: Value, args: []const Expr) InterpreterError!Value {
         switch (callee) {
            .NativeFunction => |native| {
                var evaluatedArgs = std.ArrayList(Value){};
                defer evaluatedArgs.deinit(self.allocator);
                for (args) |argExpr| {
                    const val = try self.evaluate(argExpr);
                    try evaluatedArgs.append(self.allocator, val);
                }
                return native(self.allocator, evaluatedArgs.items);
            },
            .Function => |func| {
                if (std.mem.eql(u8, func.name, "__file_read__")) {
                    const filePtr = @as(*std.fs.File, @ptrCast(@alignCast(func.closure)));
                    const content = filePtr.readToEndAlloc(self.allocator, 1024 * 1024 * 10) catch return InterpreterError.RuntimeError;
                    return Value{ .String = content };
                }
                if (std.mem.eql(u8, func.name, "__file_write__")) {
                    if (args.len != 1) return InterpreterError.RuntimeError;
                    const val = try self.evaluate(args[0]);
                    if (val != .String) return InterpreterError.TypeError;
                    const filePtr = @as(*std.fs.File, @ptrCast(@alignCast(func.closure)));
                    filePtr.writeAll(val.String) catch return InterpreterError.RuntimeError;
                    return Value{ .Nil = {} };
                }
                if (std.mem.eql(u8, func.name, "__file_close__")) {
                    const filePtr = @as(*std.fs.File, @ptrCast(@alignCast(func.closure)));
                    filePtr.close();
                    return Value{ .Nil = {} };
                }

                if (std.mem.eql(u8, func.name, "__str_strip__")) {
                    if (args.len != 0) return InterpreterError.RuntimeError;
                    const strPtr = @as(*[]const u8, @ptrCast(@alignCast(func.closure)));
                    const trimmed = std.mem.trim(u8, strPtr.*, " \t\r\n");
                    return Value{ .String = try self.allocator.dupe(u8, trimmed) };
                }
                if (std.mem.eql(u8, func.name, "__str_split__")) {
                    if (args.len != 1) return InterpreterError.RuntimeError;
                    const strPtr = @as(*[]const u8, @ptrCast(@alignCast(func.closure)));
                    const delimVal = try self.evaluate(args[0]);
                    if (delimVal != .String) return InterpreterError.TypeError;

                    var parts = std.ArrayList(Value){};
                    // Use splitSequence for string delimiter support
                    var iter = std.mem.splitSequence(u8, strPtr.*, delimVal.String);
                    while (iter.next()) |part| {
                        try parts.append(self.allocator, Value{ .String = try self.allocator.dupe(u8, part) });
                    }
                    
                    const list = try self.allocator.create(AST.LoxList);
                    list.* = AST.LoxList{ .elements = parts };
                    return Value{ .List = list };
                }

                if (std.mem.eql(u8, func.name, "__list_append__")) {
                    if (args.len != 1) return InterpreterError.RuntimeError;
                    const list = @as(*AST.LoxList, @ptrCast(@alignCast(func.closure)));
                    const val = try self.evaluate(args[0]);
                    try list.elements.append(self.allocator, val);
                    return Value{ .Nil = {} };
                }

                var expected_params = func.params.len;
                var param_offset: usize = 0;

                var closure_env: ?*Environment = null;
                if (func.closure) |c_opaque| {
                    closure_env = @as(*Environment, @ptrCast(@alignCast(c_opaque)));
                }

                var actual_parent_env: *Environment = self.globals;
                if (closure_env) |env| {
                    actual_parent_env = env;
                    if (env.values.contains("this")) {
                        expected_params -= 1;
                        param_offset = 1;
                    }
                }

                if (args.len != expected_params) {
                    std.debug.print("Expected {d} arguments but got {d}.\n", .{expected_params, args.len});
                    return InterpreterError.RuntimeError;
                }

                const fnEnv = Environment.init(self.allocator, actual_parent_env) catch return InterpreterError.OutOfMemory; 

                // If it's a bound method, the first parameter is 'self', and it's the instance.
                if (param_offset == 1) { // Means it's a bound method, and we skipped 'self' param in check
                    // The 'self' parameter needs to be defined as the instance itself.
                    // Get the instance from actual_parent_env's "this"
                    // It cannot be null if param_offset == 1, as that implies func.closure was not null.
                    const instance_val = try actual_parent_env.get("this"); // Now safe to use
                    try fnEnv.define(func.params[0], instance_val);
                }

                for (args, 0..) |arg_expr, i| {
                    const argVal = try self.evaluate(arg_expr);
                    try fnEnv.define(func.params[param_offset + i], argVal);
                }

                const ret = try self.executeBlock(func.body, fnEnv);
                if (ret) |val| return val;
                return Value{ .Nil = {} };
            },
            .Class => |klass| {
                const instance = try self.allocator.create(AST.LoxInstance);
                instance.* = AST.LoxInstance{
                    .klass = klass,
                    .fields = std.StringHashMap(Value).init(self.allocator),
                };
                const instVal = Value{ .Instance = instance };
                
                if (klass.methods.get("__init__")) |initializer| {
                    const boundInit = try self.bindMethod(instance, initializer);
                    _ = try self.callValue(boundInit, args);
                }
                
                return instVal;
            },
            else => {
                return InterpreterError.RuntimeError;
            }
        }
    }

    fn bindMethod(self: *Interpreter, instance: *AST.LoxInstance, method: Value) !Value {
        const closure = @as(*Environment, @ptrCast(@alignCast(method.Function.closure.?)));
        const env = try Environment.init(self.allocator, closure);
        try env.define("this", Value{ .Instance = instance });
        
        return Value{ .Function = .{ 
            .name = method.Function.name,
            .params = method.Function.params,
            .body = method.Function.body,
            .closure = env,
        }};
    }

    fn isTruthy(self: *Interpreter, value: Value) bool {
        _ = self;
        switch (value) {
            .Nil => return false,
            .Boolean => |b| return b,
            .Number => |n| return n != 0,
            .String => |s| return s.len > 0,
            .Function => return true,
            .NativeFunction => return true,
            .Class => return true,
            .Instance => return true,
            .List => |l| return l.elements.items.len > 0,
            .File => return true,
            .Dict => |d| return d.count() > 0,
            .Module => return true,
        }
    }

    fn isEqual(self: *Interpreter, a: Value, b: Value) bool {
        _ = self;
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        switch (a) {
             .Nil => return true,
             .Boolean => return a.Boolean == b.Boolean,
             .Number => return a.Number == b.Number,
             .String => return std.mem.eql(u8, a.String, b.String), 
             .Function => return false, 
             .NativeFunction => return false,
             .Class => |c1| return c1 == b.Class,
             .Instance => |inst1| return inst1 == b.Instance,
             .List => |l1| return l1 == b.List, // Identity check
             .File => |f1| return f1.handle == b.File.handle,
             .Dict => |d1| return d1 == b.Dict,
             .Module => |m1| return m1.exports == b.Module.exports,
        }
    }
};
