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
    // We will use a special way to handle returns or just manage it via return types
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
        // Implicit global declaration in Python if not found? 
        // No, typically UnboundLocalError or NameError.
        // But for simplicity, let's just define it in current scope if it's a global assignment?
        // Actually, Python behavior: assignment defines in local scope unless 'global' keyword used.
        // Since we don't have 'global' keyword, we'll treat all assignments as definitions in current scope.
        // Wait, AST 'Var' is used for assignment in my parser.
        // So Stmt.Var should just call define().
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
        // We need to free all environments. 
        // Simple GC or manual management? 
        // For this simple impl, we might leak if we don't track them.
        // But 'globals' is the root.
        // 'environment' points to current.
        // We should traverse up to globals or keep a list.
        // For MVP, just deinit globals (and accept leaks for now or handle better).
        // Ideally, we deinit when scope ends.
        self.globals.deinit();
    }

    pub fn interpret(self: *Interpreter, statements: []const Stmt) !void {
        for (statements) |stmt| {
            _ = try self.execute(stmt);
        }
    }

    // Returns ?Value. If non-null, it's a return value.
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
                    // .closure = self.environment, // TODO: Capture closure if we want proper closures
                }};
                try self.environment.define(s.name, func);
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
            // env.deinit(); // Can be dealloc here, assuming no closures escaping?
            // If closures escape, we can't dealloc. For now, let's leak to be safe or investigate.
            // In a real language, GC handles this.
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
                            // TODO concat
                            return InterpreterError.TypeError; 
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
                
                // Check if it's a function
                switch (callee) {
                    .Function => |func| {
                        if (c.arguments.len != func.params.len) {
                            std.debug.print("Expected {d} arguments but got {d}.\n", .{func.params.len, c.arguments.len});
                            return InterpreterError.RuntimeError;
                        }

                        // Create environment for function call
                        // Use globals as parent for now to simulate simple scoping (or capture closure if implemented)
                        // Ideally: parent = func.closure
                        const fnEnv = Environment.init(self.allocator, self.globals) catch return InterpreterError.OutOfMemory; 

                        for (func.params, 0..) |param, i| {
                            const argVal = try self.evaluate(c.arguments[i]);
                            try fnEnv.define(param, argVal);
                        }

                        const ret = try self.executeBlock(func.body, fnEnv);
                        if (ret) |val| return val;
                        return Value{ .Nil = {} };
                    },
                    else => {
                        std.debug.print("Can only call functions.\n", .{});
                        return InterpreterError.RuntimeError;
                    }
                }
            },
            .Variable => |name| {
                return self.environment.get(name);
            }
        }
    }

    fn isTruthy(self: *Interpreter, value: Value) bool {
        _ = self;
        switch (value) {
            .Nil => return false,
            .Boolean => |b| return b,
            .Number => |n| return n != 0,
            .String => |s| return s.len > 0,
            .Function => return true,
        }
    }

    fn isEqual(self: *Interpreter, a: Value, b: Value) bool {
        _ = self;
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        switch (a) {
             .Nil => return true,
             .Boolean => return a.Boolean == b.Boolean,
             .Number => return a.Number == b.Number,
             .String => return std.mem.eql(u8, a.String, b.String), // TODO string interning or full compare
             .Function => return false, // Functions are unequal unless same ref?
        }
    }
};