const std = @import("std");
const AST = @import("ast.zig");
const Expr = AST.Expr;
const Stmt = AST.Stmt;
const Value = AST.Value;
const BinaryOp = AST.BinaryOp;

pub const InterpreterError = error{
    RuntimeError,
    TypeError,
    UndefinedVariable,
    Return, 
    OutOfMemory,
};

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

pub const Interpreter = struct {
    globals: *Environment,
    environment: *Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Interpreter {
        const globals = try Environment.init(allocator, null);
        return Interpreter{
            .globals = globals,
            .environment = globals,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.globals.deinit();
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
            .Return => |s| {
                if (s.value) |expr| {
                    return try self.evaluate(expr);
                }
                return Value{ .Nil = {} };
            }
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
            }
        }
    }

    fn callValue(self: *Interpreter, callee: Value, args: []const Expr) InterpreterError!Value {
         switch (callee) {
            .Function => |func| {
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
            .Class => return true,
            .Instance => return true,
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
             .Class => |c1| return c1 == b.Class,
             .Instance => |inst1| return inst1 == b.Instance,
        }
    }
};
