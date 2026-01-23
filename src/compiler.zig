const std = @import("std");
const AST = @import("ast.zig");
const Bytecode = @import("bytecode.zig");

const Expr = AST.Expr;
const Stmt = AST.Stmt;
const Value = AST.Value;
const OpCode = Bytecode.OpCode;
const Chunk = Bytecode.Chunk;
const BytecodeFunction = Bytecode.BytecodeFunction;

pub const CompileError = error{
    OutOfMemory,
    UnsupportedFeature,
};

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    chunk: *Chunk,
    max_locals: usize,
    temp_next: usize,

    pub fn init(allocator: std.mem.Allocator, base_locals: usize) Compiler {
        return .{
            .allocator = allocator,
            .chunk = undefined,
            .max_locals = base_locals,
            .temp_next = base_locals,
        };
    }

    pub fn compileScript(self: *Compiler, statements: []Stmt) CompileError!*BytecodeFunction {
        const func = try self.allocator.create(BytecodeFunction);
        func.* = .{
            .name = "<script>",
            .chunk = Chunk.init(),
            .arity = 0,
            .locals_count = 0,
            .this_slot = -1,
        };
        self.chunk = &func.chunk;

        const stmts = @constCast(statements);
        for (stmts, 0..) |_, i| {
            try self.compileStmt(&stmts[i]);
        }
        try self.emitOp(.Nil);
        try self.emitOp(.Return);
        func.locals_count = self.max_locals;
        return func;
    }

    pub fn compileFunction(self: *Compiler, stmt: *Stmt) CompileError!*BytecodeFunction {
        if (stmt.* != .Function) return CompileError.UnsupportedFeature;
        const f = stmt.Function;
        const func = try self.allocator.create(BytecodeFunction);
        func.* = .{
            .name = f.name,
            .chunk = Chunk.init(),
            .arity = @as(u8, @intCast(f.params.len)),
            .locals_count = 0,
            .this_slot = f.this_slot,
        };

        var child = Compiler.init(self.allocator, f.locals_count);
        child.chunk = &func.chunk;
        const body = @constCast(f.body);
        for (body, 0..) |_, i| {
            try child.compileStmt(&body[i]);
        }
        try child.emitOp(.Nil);
        try child.emitOp(.Return);
        func.locals_count = child.max_locals;
        return func;
    }

    fn allocTemp(self: *Compiler) usize {
        const slot = self.temp_next;
        self.temp_next += 1;
        if (self.temp_next > self.max_locals) self.max_locals = self.temp_next;
        return slot;
    }

    fn emitOp(self: *Compiler, op: OpCode) CompileError!void {
        try self.chunk.writeOp(self.allocator, op);
    }

    fn emitU8(self: *Compiler, value: u8) CompileError!void {
        try self.chunk.writeU8(self.allocator, value);
    }

    fn emitU16(self: *Compiler, value: u16) CompileError!void {
        try self.chunk.writeU16(self.allocator, value);
    }

    fn emitConstant(self: *Compiler, value: Value) CompileError!void {
        const idx = try self.chunk.addConstant(self.allocator, value);
        try self.emitOp(.Constant);
        try self.emitU16(idx);
    }

    fn emitJump(self: *Compiler, op: OpCode) CompileError!usize {
        try self.emitOp(op);
        const jump_pos = self.chunk.code.items.len;
        try self.emitU16(0);
        return jump_pos;
    }

    fn patchJump(self: *Compiler, jump_pos: usize) void {
        const offset = self.chunk.code.items.len - jump_pos - 2;
        self.chunk.code.items[jump_pos] = @as(u8, @intCast((offset >> 8) & 0xff));
        self.chunk.code.items[jump_pos + 1] = @as(u8, @intCast(offset & 0xff));
    }

    fn emitLoop(self: *Compiler, loop_start: usize) CompileError!void {
        try self.emitOp(.Loop);
        const offset = self.chunk.code.items.len - loop_start + 2;
        try self.emitU16(@as(u16, @intCast(offset)));
    }

    fn compileStmt(self: *Compiler, stmt: *Stmt) CompileError!void {
        switch (stmt.*) {
            .Expression => |*s| {
                try self.compileExpr(&s.expression);
                try self.emitOp(.Pop);
            },
            .Print => |*s| {
                try self.compileExpr(&s.expression);
                try self.emitOp(.Print);
            },
            .Var => |*s| {
                try self.compileExpr(&s.initializer);
                if (s.slot >= 0) {
                    try self.emitOp(.SetLocal);
                    try self.emitU16(@as(u16, @intCast(s.slot)));
                    try self.emitOp(.Pop);
                } else {
                    const idx = try self.chunk.addConstant(self.allocator, Value{ .String = s.name });
                    try self.emitOp(.DefineGlobal);
                    try self.emitU16(idx);
                }
            },
            .Block => |*b| {
                const stmts = @constCast(b.statements);
                for (stmts, 0..) |_, i| {
                    try self.compileStmt(&stmts[i]);
                }
            },
            .If => |*s| {
                try self.compileExpr(&s.condition);
                const else_jump = try self.emitJump(.JumpIfFalse);
                try self.emitOp(.Pop);
                try self.compileStmt(@constCast(s.then_branch));
                const end_jump = try self.emitJump(.Jump);
                self.patchJump(else_jump);
                try self.emitOp(.Pop);
                if (s.else_branch) |else_branch| {
                    try self.compileStmt(@constCast(else_branch));
                }
                self.patchJump(end_jump);
            },
            .While => |*s| {
                const loop_start = self.chunk.code.items.len;
                try self.compileExpr(&s.condition);
                const exit_jump = try self.emitJump(.JumpIfFalse);
                try self.emitOp(.Pop);
                try self.compileStmt(@constCast(s.body));
                try self.emitLoop(loop_start);
                self.patchJump(exit_jump);
                try self.emitOp(.Pop);
            },
            .For => |*f| {
                try self.compileExpr(&f.iterable);
                const list_slot = self.allocTemp();
                const idx_slot = self.allocTemp();

                try self.emitOp(.SetLocal);
                try self.emitU16(@as(u16, @intCast(list_slot)));
                try self.emitOp(.Pop);

                try self.emitConstant(Value{ .Number = 0 });
                try self.emitOp(.SetLocal);
                try self.emitU16(@as(u16, @intCast(idx_slot)));
                try self.emitOp(.Pop);

                const loop_start = self.chunk.code.items.len;
                try self.emitOp(.GetLocal);
                try self.emitU16(@as(u16, @intCast(idx_slot)));
                try self.emitOp(.GetLocal);
                try self.emitU16(@as(u16, @intCast(list_slot)));
                try self.emitOp(.ListLen);
                try self.emitOp(.Less);

                const exit_jump = try self.emitJump(.JumpIfFalse);
                try self.emitOp(.Pop);

                try self.emitOp(.GetLocal);
                try self.emitU16(@as(u16, @intCast(list_slot)));
                try self.emitOp(.GetLocal);
                try self.emitU16(@as(u16, @intCast(idx_slot)));
                try self.emitOp(.GetSubscript);

                if (f.slot >= 0) {
                    try self.emitOp(.SetLocal);
                    try self.emitU16(@as(u16, @intCast(f.slot)));
                    try self.emitOp(.Pop);
                } else {
                    const name_idx = try self.chunk.addConstant(self.allocator, Value{ .String = f.variable });
                    try self.emitOp(.DefineGlobal);
                    try self.emitU16(name_idx);
                }

                try self.compileStmt(@constCast(f.body));

                try self.emitOp(.GetLocal);
                try self.emitU16(@as(u16, @intCast(idx_slot)));
                try self.emitConstant(Value{ .Number = 1 });
                try self.emitOp(.Add);
                try self.emitOp(.SetLocal);
                try self.emitU16(@as(u16, @intCast(idx_slot)));
                try self.emitOp(.Pop);

                try self.emitLoop(loop_start);
                self.patchJump(exit_jump);
                try self.emitOp(.Pop);
            },
            .Function => |*f| {
                const func = try self.compileFunction(stmt);
                try self.emitConstant(Value{ .BytecodeFunction = @as(*anyopaque, @ptrCast(func)) });
                if (f.slot >= 0) {
                    try self.emitOp(.SetLocal);
                    try self.emitU16(@as(u16, @intCast(f.slot)));
                    try self.emitOp(.Pop);
                } else {
                    const name_idx = try self.chunk.addConstant(self.allocator, Value{ .String = f.name });
                    try self.emitOp(.DefineGlobal);
                    try self.emitU16(name_idx);
                }
            },
            .Return => |*r| {
                if (r.value) |*val| {
                    try self.compileExpr(val);
                } else {
                    try self.emitOp(.Nil);
                }
                try self.emitOp(.Return);
            },
            .Class, .Import, .Raise, .Try => return CompileError.UnsupportedFeature,
        }
    }

    fn compileExpr(self: *Compiler, expr: *Expr) CompileError!void {
        switch (expr.*) {
            .Literal => |v| switch (v) {
                .Nil => try self.emitOp(.Nil),
                .Boolean => |b| try self.emitOp(if (b) .True else .False),
                else => try self.emitConstant(v),
            },
            .Grouping => |g| try self.compileExpr(@constCast(g)),
            .Unary => |*u| {
                try self.compileExpr(@constCast(u.right));
                switch (u.op) {
                    .Negate => try self.emitOp(.Negate),
                    .Not => try self.emitOp(.Not),
                }
            },
            .Binary => |*b| {
                try self.compileExpr(@constCast(b.left));
                try self.compileExpr(@constCast(b.right));
                const op = switch (b.op) {
                    .Add => OpCode.Add,
                    .Sub => OpCode.Subtract,
                    .Mul => OpCode.Multiply,
                    .Div => OpCode.Divide,
                    .Equal => OpCode.Equal,
                    .NotEqual => OpCode.NotEqual,
                    .Greater => OpCode.Greater,
                    .GreaterEqual => OpCode.GreaterEqual,
                    .Less => OpCode.Less,
                    .LessEqual => OpCode.LessEqual,
                };
                try self.emitOp(op);
            },
            .Logical => |*l| {
                try self.compileExpr(@constCast(l.left));
                if (l.op == .Or) {
                    const else_jump = try self.emitJump(.JumpIfFalse);
                    const end_jump = try self.emitJump(.Jump);
                    self.patchJump(else_jump);
                    try self.emitOp(.Pop);
                    try self.compileExpr(@constCast(l.right));
                    self.patchJump(end_jump);
                } else {
                    const end_jump = try self.emitJump(.JumpIfFalse);
                    try self.emitOp(.Pop);
                    try self.compileExpr(@constCast(l.right));
                    self.patchJump(end_jump);
                }
            },
            .Call => |*c| {
                try self.compileExpr(@constCast(c.callee));
                const args = @constCast(c.arguments);
                for (args, 0..) |_, i| {
                    try self.compileExpr(&args[i]);
                }
                try self.emitOp(.Call);
                try self.emitU8(@as(u8, @intCast(args.len)));
            },
            .Get, .Set => return CompileError.UnsupportedFeature,
            .This => |t| {
                if (t.depth > 0) return CompileError.UnsupportedFeature;
                if (t.depth == 0) {
                    try self.emitOp(.GetLocal);
                    try self.emitU16(@as(u16, @intCast(t.slot)));
                } else {
                    const idx = try self.chunk.addConstant(self.allocator, Value{ .String = t.keyword });
                    try self.emitOp(.GetGlobal);
                    try self.emitU16(idx);
                }
            },
            .Variable => |v| {
                if (v.depth > 0) return CompileError.UnsupportedFeature;
                if (v.depth == 0) {
                    try self.emitOp(.GetLocal);
                    try self.emitU16(@as(u16, @intCast(v.slot)));
                } else {
                    const idx = try self.chunk.addConstant(self.allocator, Value{ .String = v.name });
                    try self.emitOp(.GetGlobal);
                    try self.emitU16(idx);
                }
            },
            .ListLiteral => |*l| {
                const elems = @constCast(l.elements);
                for (elems, 0..) |_, i| {
                    try self.compileExpr(&elems[i]);
                }
                try self.emitOp(.BuildList);
                try self.emitU16(@as(u16, @intCast(elems.len)));
            },
            .DictLiteral => return CompileError.UnsupportedFeature,
            .Subscript => |*s| {
                try self.compileExpr(@constCast(s.value));
                try self.compileExpr(@constCast(s.index));
                try self.emitOp(.GetSubscript);
            },
            .SetSubscript => |*s| {
                try self.compileExpr(@constCast(s.object));
                try self.compileExpr(@constCast(s.index));
                try self.compileExpr(@constCast(s.value));
                try self.emitOp(.SetSubscript);
            },
            .FString => return CompileError.UnsupportedFeature,
        }
    }
};
