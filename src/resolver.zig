const std = @import("std");
const AST = @import("ast.zig");
const Expr = AST.Expr;
const Stmt = AST.Stmt;

const ResolveError = error{OutOfMemory};

const Scope = struct {
    map: std.StringHashMap(usize),
    next_slot: usize,
};

pub const Resolver = struct {
    allocator: std.mem.Allocator,
    scopes: std.ArrayList(Scope),

    pub fn init(allocator: std.mem.Allocator) Resolver {
        return .{
            .allocator = allocator,
            .scopes = std.ArrayList(Scope){},
        };
    }

    pub fn deinit(self: *Resolver) void {
        for (self.scopes.items) |*scope| {
            scope.map.deinit();
        }
        self.scopes.deinit(self.allocator);
    }

    pub fn resolve(self: *Resolver, statements: []Stmt) ResolveError!void {
        for (statements, 0..) |_, i| {
            try self.resolveStmt(&statements[i]);
        }
    }

    fn beginScope(self: *Resolver) ResolveError!void {
        const scope = Scope{
            .map = std.StringHashMap(usize).init(self.allocator),
            .next_slot = 0,
        };
        try self.scopes.append(self.allocator, scope);
    }

    fn endScope(self: *Resolver) void {
        var scope = self.scopes.pop() orelse return;
        scope.map.deinit();
    }

    fn define(self: *Resolver, name: []const u8) ResolveError!i32 {
        if (self.scopes.items.len == 0) return -1;
        var scope = &self.scopes.items[self.scopes.items.len - 1];
        if (scope.map.get(name)) |slot| {
            return @as(i32, @intCast(slot));
        }
        const slot = scope.next_slot;
        scope.next_slot += 1;
        try scope.map.put(name, slot);
        return @as(i32, @intCast(slot));
    }

    const LocalInfo = struct {
        depth: i32,
        slot: i32,
    };

    fn resolveLocal(self: *Resolver, name: []const u8) LocalInfo {
        var i: isize = @as(isize, @intCast(self.scopes.items.len)) - 1;
        while (i >= 0) : (i -= 1) {
            const idx = @as(usize, @intCast(i));
            if (self.scopes.items[idx].map.get(name)) |slot| {
                return .{
                    .depth = @as(i32, @intCast(self.scopes.items.len - 1 - idx)),
                    .slot = @as(i32, @intCast(slot)),
                };
            }
        }
        return .{ .depth = -1, .slot = -1 };
    }

    fn resolveFunction(self: *Resolver, params: []const []const u8, body: []const Stmt, with_this: bool) ResolveError!struct { locals_count: usize, this_slot: i32 } {
        try self.beginScope();
        defer self.endScope();

        var this_slot: i32 = -1;
        if (with_this) {
            this_slot = try self.define("this");
        }
        for (params) |param| {
            _ = try self.define(param);
        }

        const body_mut = @constCast(body);
        for (body_mut, 0..) |_, i| {
            try self.resolveStmt(&body_mut[i]);
        }

        const locals_count = self.scopes.items[self.scopes.items.len - 1].next_slot;
        return .{ .locals_count = locals_count, .this_slot = this_slot };
    }

    fn resolveStmt(self: *Resolver, stmt: *Stmt) ResolveError!void {
        switch (stmt.*) {
            .Expression => |*s| {
                try self.resolveExpr(&s.expression);
            },
            .Print => |*s| {
                try self.resolveExpr(&s.expression);
            },
            .Var => |*s| {
                try self.resolveExpr(&s.initializer);
                s.slot = try self.define(s.name);
            },
            .Block => |*b| {
                const stmts = @constCast(b.statements);
                for (stmts, 0..) |_, i| {
                    try self.resolveStmt(&stmts[i]);
                }
            },
            .If => |*s| {
                try self.resolveExpr(&s.condition);
                try self.resolveStmt(@constCast(s.then_branch));
                if (s.else_branch) |else_branch| {
                    try self.resolveStmt(@constCast(else_branch));
                }
            },
            .While => |*s| {
                try self.resolveExpr(&s.condition);
                try self.resolveStmt(@constCast(s.body));
            },
            .For => |*f| {
                try self.resolveExpr(&f.iterable);
                f.slot = try self.define(f.variable);
                try self.resolveStmt(@constCast(f.body));
            },
            .Function => |*f| {
                f.slot = try self.define(f.name);
                const info = try self.resolveFunction(f.params, f.body, false);
                f.locals_count = info.locals_count;
                f.this_slot = info.this_slot;
            },
            .Class => |*c| {
                _ = try self.define(c.name);
                const methods = @constCast(c.methods);
                for (methods, 0..) |_, i| {
                    const method_stmt = &methods[i];
                    switch (method_stmt.*) {
                        .Function => |*f| {
                            const info = try self.resolveFunction(f.params, f.body, true);
                            f.locals_count = info.locals_count;
                            f.this_slot = info.this_slot;
                        },
                        else => {},
                    }
                }
            },
            .Return => |*r| {
                if (r.value) |*val| {
                    try self.resolveExpr(val);
                }
            },
            .Import => |_| {},
            .Raise => |*r| {
                try self.resolveExpr(&r.value);
            },
            .Try => |*t| {
                try self.resolveStmt(@constCast(t.try_branch));
                try self.resolveStmt(@constCast(t.except_branch));
            },
        }
    }

    fn resolveExpr(self: *Resolver, expr: *Expr) ResolveError!void {
        switch (expr.*) {
            .Binary => |*b| {
                try self.resolveExpr(@constCast(b.left));
                try self.resolveExpr(@constCast(b.right));
            },
            .Unary => |*u| {
                try self.resolveExpr(@constCast(u.right));
            },
            .Logical => |*l| {
                try self.resolveExpr(@constCast(l.left));
                try self.resolveExpr(@constCast(l.right));
            },
            .Call => |*c| {
                try self.resolveExpr(@constCast(c.callee));
                const args = @constCast(c.arguments);
                for (args, 0..) |_, i| {
                    try self.resolveExpr(&args[i]);
                }
            },
            .Get => |*g| {
                try self.resolveExpr(@constCast(g.object));
            },
            .Set => |*s| {
                try self.resolveExpr(@constCast(s.object));
                try self.resolveExpr(@constCast(s.value));
            },
            .This => |*t| {
                const info = self.resolveLocal(t.keyword);
                t.depth = info.depth;
                t.slot = info.slot;
            },
            .ListLiteral => |*l| {
                const elems = @constCast(l.elements);
                for (elems, 0..) |_, i| {
                    try self.resolveExpr(&elems[i]);
                }
            },
            .DictLiteral => |*d| {
                const keys = @constCast(d.keys);
                const values = @constCast(d.values);
                for (keys, 0..) |_, i| {
                    try self.resolveExpr(&keys[i]);
                    try self.resolveExpr(&values[i]);
                }
            },
            .Subscript => |*s| {
                try self.resolveExpr(@constCast(s.value));
                try self.resolveExpr(@constCast(s.index));
            },
            .SetSubscript => |*s| {
                try self.resolveExpr(@constCast(s.object));
                try self.resolveExpr(@constCast(s.index));
                try self.resolveExpr(@constCast(s.value));
            },
            .Literal => |_| {},
            .FString => |*f| {
                const parts = @constCast(f.parts);
                for (parts, 0..) |_, i| {
                    try self.resolveExpr(&parts[i]);
                }
            },
            .Grouping => |g| {
                try self.resolveExpr(@constCast(g));
            },
            .Variable => |*v| {
                const info = self.resolveLocal(v.name);
                v.depth = info.depth;
                v.slot = info.slot;
            },
        }
    }
};
