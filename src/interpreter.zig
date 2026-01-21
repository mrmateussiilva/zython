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
};

pub const Interpreter = struct {
    environment: std.StringHashMap(Value),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        return Interpreter{
            .environment = std.StringHashMap(Value).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.environment.deinit();
    }

    pub fn interpret(self: *Interpreter, statements: []const Stmt) !void {
        for (statements) |stmt| {
            try self.execute(stmt);
        }
    }

    fn execute(self: *Interpreter, stmt: Stmt) !void {
        switch (stmt) {
            .Print => |s| {
                const val = try self.evaluate(s.expression);
                const str = try val.toString(self.allocator);
                std.debug.print("{s}\n", .{str});
            },
            .Expression => |s| {
                _ = try self.evaluate(s.expression);
            },
            .Var => |s| {
                const val = try self.evaluate(s.initializer);
                try self.environment.put(s.name, val);
            },
            .Block => |b| {
                // TODO: Handle scoping
                for (b.statements) |st| {
                    try self.execute(st);
                }
            }
        }
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
                         // TODO: Python rules for truthiness
                         if (right == .Boolean) return Value{ .Boolean = !right.Boolean };
                         return InterpreterError.TypeError;
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
                            // Concat string TODO
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
                        // Simples comparação
                        if (std.meta.activeTag(left) != std.meta.activeTag(right)) return Value{ .Boolean = false };
                        switch (left) {
                            .Number => return Value{ .Boolean = left.Number == right.Number },
                            .Boolean => return Value{ .Boolean = left.Boolean == right.Boolean },
                            else => return Value{ .Boolean = false }, // TODO complete eq
                        }
                    },
                    .NotEqual => {
                         if (std.meta.activeTag(left) != std.meta.activeTag(right)) return Value{ .Boolean = true };
                         switch (left) {
                             .Number => return Value{ .Boolean = left.Number != right.Number },
                             .Boolean => return Value{ .Boolean = left.Boolean != right.Boolean },
                             else => return Value{ .Boolean = true },
                         }
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
            .Variable => |name| {
                if (self.environment.get(name)) |val| {
                    return val;
                }
                std.debug.print("Undefined variable '{s}'\n", .{name});
                return InterpreterError.UndefinedVariable;
            }
        }
    }
};
