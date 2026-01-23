const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Lexer = @import("lexer.zig").Lexer;
const AST = @import("ast.zig");
const Expr = AST.Expr;
const Stmt = AST.Stmt;
const Value = AST.Value;

pub const ParserError = error{
    UnexpectedToken,
    ExpectedExpression,
    OutOfMemory,
};

pub const Parser = struct {
    lexer: *Lexer,
    current: Token,
    previous: Token,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) !Parser {
        var p = Parser{
            .lexer = lexer,
            .current = undefined,
            .previous = undefined,
            .allocator = allocator,
        };
        try p.advance();
        return p;
    }

    fn advance(self: *Parser) !void {
        self.previous = self.current;
        while (true) {
            self.current = try self.lexer.nextToken();
            if (self.current.type != .Error) break;
            // TODO: Report error
        }
    }

    fn check(self: *Parser, type_: TokenType) bool {
        return self.current.type == type_;
    }

    fn match(self: *Parser, types: []const TokenType) !bool {
        for (types) |t| {
            if (self.check(t)) {
                try self.advance();
                return true;
            }
        }
        return false;
    }

    fn consume(self: *Parser, type_: TokenType, message: []const u8) !Token {
        if (self.check(type_)) {
            try self.advance();
            return self.previous;
        }
        std.debug.print("Error at {d}:{d} - {s}. Found {any}\n", .{self.current.line, self.current.col, message, self.current.type});
        return ParserError.UnexpectedToken;
    }

    // --- Grammar Rules ---

    pub fn parse(self: *Parser) !std.ArrayList(Stmt) {
        var statements = std.ArrayList(Stmt){};
        while (!self.check(.Eof)) {
            if (try self.match(&.{.Newline})) continue;
            const decl = try self.declaration();
            try statements.append(self.allocator, decl);
        }
        return statements;
    }

    fn declaration(self: *Parser) ParserError!Stmt {
        if (try self.match(&.{.Class})) return try self.classDeclaration();
        if (try self.match(&.{.Fun})) return try self.function("function");
        if (try self.match(&.{.Import})) {
             const name = try self.consume(.Identifier, "Expect module name.");
             return Stmt{ .Import = .{ .name = name.lexeme, .slot = -1 } };
        }
        return try self.statement();
    }

    fn classDeclaration(self: *Parser) ParserError!Stmt {
        const name = try self.consume(.Identifier, "Expect class name.");
        // TODO: Inheritance (LeftParen)
        _ = try self.consume(.Colon, "Expect ':' before class body.");

        const body = try self.block();

        return Stmt{ .Class = .{
            .name = name.lexeme,
            .methods = body,
        }};
    }

    fn function(self: *Parser, kind: []const u8) ParserError!Stmt {
        _ = kind;
        const name = try self.consume(.Identifier, "Expect function name.");
        _ = try self.consume(.LeftParen, "Expect \'(\' after function name.");
        
        var params = std.ArrayList([]const u8){};
        
        if (!self.check(.RightParen)) {
            while (true) {
                const param = try self.consume(.Identifier, "Expect parameter name.");
                try params.append(self.allocator, param.lexeme);
                if (!try self.match(&.{.Comma})) break;
            }
        }
        _ = try self.consume(.RightParen, "Expect ')' after parameters.");
        
        _ = try self.consume(.Colon, "Expect ':' before function body.");
        
        const body = try self.block();
        
        return Stmt{ .Function = .{
            .name = name.lexeme,
            .params = params.toOwnedSlice(self.allocator) catch return ParserError.OutOfMemory,
            .body = body,
            .locals_count = 0,
            .this_slot = -1,
            .slot = -1,
        }};
    }

    fn statement(self: *Parser) ParserError!Stmt {
        if (try self.match(&.{.For})) return try self.forStatement();
        if (try self.match(&.{.If})) return try self.ifStatement();
        if (try self.match(&.{.Print})) return try self.printStatement();
        if (try self.match(&.{.Raise})) return try self.raiseStatement();
        if (try self.match(&.{.Return})) return try self.returnStatement();
        if (try self.match(&.{.Try})) return try self.tryStatement();
        if (try self.match(&.{.While})) return try self.whileStatement();
        if (try self.match(&.{.LeftBrace})) {
            const stmts = try self.block();
            return Stmt{ .Block = .{ .statements = stmts } };
        }
        
        return try self.expressionStatement();
    }

    fn ifStatement(self: *Parser) ParserError!Stmt {
        const condition = try self.expression();
        _ = try self.consume(.Colon, "Expect ':' after if condition.");
        
        const thenBranchStatements = try self.block();
        const thenBranch = try self.allocator.create(Stmt);
        thenBranch.* = Stmt{ .Block = .{ .statements = thenBranchStatements } };

        var elseBranch: ?*const Stmt = null;
        if (try self.match(&.{.Else})) {
            _ = try self.consume(.Colon, "Expect ':' after else.");
            const elseBranchStatements = try self.block();
            const elsePtr = try self.allocator.create(Stmt);
            elsePtr.* = Stmt{ .Block = .{ .statements = elseBranchStatements } };
            elseBranch = elsePtr;
        }

        return Stmt{ .If = .{
            .condition = condition,
            .then_branch = thenBranch,
            .else_branch = elseBranch,
        }};
    }

    fn whileStatement(self: *Parser) ParserError!Stmt {
        const condition = try self.expression();
        _ = try self.consume(.Colon, "Expect ':' after while condition.");
        const bodyStatements = try self.block();
        const bodyPtr = try self.allocator.create(Stmt);
        bodyPtr.* = Stmt{ .Block = .{ .statements = bodyStatements } };

        return Stmt{ .While = .{
            .condition = condition,
            .body = bodyPtr,
        }};
    }

    fn forStatement(self: *Parser) ParserError!Stmt {
        const variable = try self.consume(.Identifier, "Expect variable name after 'for'.");
        _ = try self.consume(.In, "Expect 'in' after variable.");
        const iterable = try self.expression();
        _ = try self.consume(.Colon, "Expect ':' after iterable.");
        
        const bodyStatements = try self.block();
        const bodyPtr = try self.allocator.create(Stmt);
        bodyPtr.* = Stmt{ .Block = .{ .statements = bodyStatements } };

        return Stmt{ .For = .{
            .variable = variable.lexeme,
            .iterable = iterable,
            .body = bodyPtr,
            .slot = -1,
        }};
    }
    
    fn returnStatement(self: *Parser) ParserError!Stmt {
        var value: ?Expr = null;
        if (!self.check(.Newline)) {
            value = try self.expression();
        }
        if (self.check(.Newline)) _ = try self.advance(); // consume optional newline
        return Stmt{ .Return = .{ .value = value } };
    }

    fn block(self: *Parser) ParserError![]const Stmt {
        _ = try self.consume(.Newline, "Expect newline before block.");
        _ = try self.consume(.Indent, "Expect indentation.");
        
        var statements = std.ArrayList(Stmt){};
        
        while (!self.check(.Dedent) and !self.check(.Eof)) {
            if (try self.match(&.{.Newline})) continue;
            const stmt = try self.declaration();
            try statements.append(self.allocator, stmt);
        }
        
        _ = try self.consume(.Dedent, "Expect dedent after block.");
        return statements.toOwnedSlice(self.allocator) catch return ParserError.OutOfMemory;
    }

    fn raiseStatement(self: *Parser) ParserError!Stmt {
        const value = try self.expression();
        if (self.check(.Newline)) _ = try self.advance();
        return Stmt{ .Raise = .{ .value = value } };
    }

    fn tryStatement(self: *Parser) ParserError!Stmt {
        _ = try self.consume(.Colon, "Expect ':' after 'try'.");
        const try_stmts = try self.block();
        const try_block = Stmt{ .Block = .{ .statements = try_stmts } };
        
        _ = try self.consume(.Except, "Expect 'except' after try block.");
        _ = try self.consume(.Colon, "Expect ':' after 'except'.");
        const except_stmts = try self.block();
        const except_block = Stmt{ .Block = .{ .statements = except_stmts } };

        const try_ptr = try self.allocator.create(Stmt);
        try_ptr.* = try_block;
        
        const except_ptr = try self.allocator.create(Stmt);
        except_ptr.* = except_block;

        return Stmt{ .Try = .{ .try_branch = try_ptr, .except_branch = except_ptr } };
    }

    fn printStatement(self: *Parser) ParserError!Stmt {
        _ = try self.consume(.LeftParen, "Expect \'(\' after print.");
        const value = try self.expression();
        _ = try self.consume(.RightParen, "Expect ')' after value.");
        if (self.check(.Newline)) _ = try self.advance();
        return Stmt{ .Print = .{ .expression = value } };
    }

    fn expressionStatement(self: *Parser) ParserError!Stmt {
        const expr = try self.expression();
        
        if (try self.match(&.{.Equal})) {
             const value = try self.expression();
             return self.finishAssignment(expr, value);
        }

        if (try self.match(&.{.PlusEqual, .MinusEqual, .StarEqual, .SlashEqual})) {
            const op_token = self.previous;
            const value = try self.expression();
            
            const left_ptr = try self.allocator.create(Expr);
            left_ptr.* = expr;
            
            const right_ptr = try self.allocator.create(Expr);
            right_ptr.* = value;
            
            const op = switch (op_token.type) {
                .PlusEqual => AST.BinaryOp.Add,
                .MinusEqual => AST.BinaryOp.Sub,
                .StarEqual => AST.BinaryOp.Mul,
                .SlashEqual => AST.BinaryOp.Div,
                else => unreachable,
            };
            
            const desugared_value = Expr{ .Binary = .{
                .left = left_ptr,
                .op = op,
                .right = right_ptr
            }};
            
            return self.finishAssignment(expr, desugared_value);
        }
        
        if (self.check(.Newline)) _ = try self.advance();
        return Stmt{ .Expression = .{ .expression = expr } };
    }

    fn finishAssignment(self: *Parser, lvalue: Expr, rvalue: Expr) ParserError!Stmt {
             switch (lvalue) {
                 .Variable => |var_expr| {
                     if (self.check(.Newline)) _ = try self.advance();
                     return Stmt{ .Var = .{ .name = var_expr.name, .initializer = rvalue, .slot = -1 } };
                 },
                 .Get => |get| {
                     const valuePtr = try self.allocator.create(Expr);
                     valuePtr.* = rvalue;
                     if (self.check(.Newline)) _ = try self.advance();
                     return Stmt{ .Expression = .{ .expression = Expr{ .Set = .{ 
                         .object = get.object, 
                         .name = get.name, 
                         .value = valuePtr 
                     }}}}; 
                 },
                 .Subscript => |subs| {
                     const valuePtr = try self.allocator.create(Expr);
                     valuePtr.* = rvalue;
                     if (self.check(.Newline)) _ = try self.advance();
                     return Stmt{ .Expression = .{ .expression = Expr{ .SetSubscript = .{
                         .object = subs.value,
                         .index = subs.index,
                         .value = valuePtr
                     }}}};
                 },
                 else => {
                     return ParserError.UnexpectedToken;
                 }
             }
    }

    fn expression(self: *Parser) ParserError!Expr {
        return self.logicOr();
    }
    
    fn logicOr(self: *Parser) ParserError!Expr {
        var expr = try self.logicAnd();
        while (try self.match(&.{.Or})) {
            const right = try self.logicAnd();
            const left_ptr = try self.allocator.create(Expr);
            left_ptr.* = expr;
            const right_ptr = try self.allocator.create(Expr);
            right_ptr.* = right;
            expr = Expr{ .Logical = .{ .left = left_ptr, .op = .Or, .right = right_ptr } };
        }
        return expr;
    }

    fn logicAnd(self: *Parser) ParserError!Expr {
        var expr = try self.equality();
        while (try self.match(&.{.And})) {
            const right = try self.equality();
            const left_ptr = try self.allocator.create(Expr);
            left_ptr.* = expr;
            const right_ptr = try self.allocator.create(Expr);
            right_ptr.* = right;
            expr = Expr{ .Logical = .{ .left = left_ptr, .op = .And, .right = right_ptr } };
        }
        return expr;
    }

    fn equality(self: *Parser) ParserError!Expr {
        var expr = try self.comparison();
        while (try self.match(&.{.EqualEqual, .BangEqual})) {
            const op_token = self.previous;
            const right = try self.comparison();
            const right_ptr = try self.allocator.create(Expr);
            right_ptr.* = right;
            const left_ptr = try self.allocator.create(Expr);
            left_ptr.* = expr;
            expr = Expr{ .Binary = .{
                .left = left_ptr,
                .op = if (op_token.type == .EqualEqual) AST.BinaryOp.Equal else AST.BinaryOp.NotEqual,
                .right = right_ptr
            }};
        }
        return expr;
    }

    fn comparison(self: *Parser) ParserError!Expr {
        var expr = try self.term();
        while (try self.match(&.{.Greater, .GreaterEqual, .Less, .LessEqual})) {
            const op_token = self.previous;
            const right = try self.term();
            const right_ptr = try self.allocator.create(Expr);
            right_ptr.* = right;
            const left_ptr = try self.allocator.create(Expr);
            left_ptr.* = expr;
            const op = switch (op_token.type) {
                .Greater => AST.BinaryOp.Greater,
                .GreaterEqual => AST.BinaryOp.GreaterEqual,
                .Less => AST.BinaryOp.Less,
                .LessEqual => AST.BinaryOp.LessEqual,
                else => unreachable,
            };
            expr = Expr{ .Binary = .{
                .left = left_ptr,
                .op = op,
                .right = right_ptr
            }};
        }
        return expr;
    }

    fn term(self: *Parser) ParserError!Expr {
        var expr = try self.factor();
        while (try self.match(&.{.Minus, .Plus})) {
            const op_token = self.previous;
            const right = try self.factor();
            const right_ptr = try self.allocator.create(Expr);
            right_ptr.* = right;
            const left_ptr = try self.allocator.create(Expr);
            left_ptr.* = expr;
            expr = Expr{ .Binary = .{
                .left = left_ptr,
                .op = if (op_token.type == .Minus) AST.BinaryOp.Sub else AST.BinaryOp.Add,
                .right = right_ptr
            }};
        }
        return expr;
    }

    fn factor(self: *Parser) ParserError!Expr {
        var expr = try self.unary();
        while (try self.match(&.{.Slash, .Star})) {
            const op_token = self.previous;
            const right = try self.unary();
            const right_ptr = try self.allocator.create(Expr);
            right_ptr.* = right;
            const left_ptr = try self.allocator.create(Expr);
            left_ptr.* = expr;
            expr = Expr{ .Binary = .{
                .left = left_ptr,
                .op = if (op_token.type == .Slash) AST.BinaryOp.Div else AST.BinaryOp.Mul,
                .right = right_ptr
            }};
        }
        return expr;
    }

    fn unary(self: *Parser) ParserError!Expr {
        if (try self.match(&.{.Bang, .Minus})) {
             const op_token = self.previous;
             const right = try self.unary();
             const right_ptr = try self.allocator.create(Expr);
             right_ptr.* = right;
             return Expr{ .Unary = .{
                 .op = if (op_token.type == .Bang) AST.UnaryOp.Not else AST.UnaryOp.Negate,
                 .right = right_ptr
             }};
        }
        return try self.call();
    }
    
    fn call(self: *Parser) ParserError!Expr {
        var expr = try self.primary();
        
        while (true) {
            if (try self.match(&.{.LeftParen})) {
                expr = try self.finishCall(expr);
            } else if (try self.match(&.{.Dot})) {
                const name = try self.consume(.Identifier, "Expect property name after '.'");
                const exprPtr = try self.allocator.create(Expr);
                exprPtr.* = expr;
                expr = Expr{ .Get = .{ .object = exprPtr, .name = name.lexeme } };
            } else if (try self.match(&.{.LeftBracket})) {
                const index = try self.expression();
                _ = try self.consume(.RightBracket, "Expect ']' after subscript.");
                const exprPtr = try self.allocator.create(Expr);
                exprPtr.* = expr;
                const indexPtr = try self.allocator.create(Expr);
                indexPtr.* = index;
                expr = Expr{ .Subscript = .{ .value = exprPtr, .index = indexPtr } };
            } else {
                break;
            }
        }
        return expr;
    }
    
    fn finishCall(self: *Parser, callee: Expr) ParserError!Expr {
        var args = std.ArrayList(Expr){};
        
        if (!self.check(.RightParen)) {
            while (true) {
                const arg = try self.expression();
                try args.append(self.allocator, arg);
                if (!try self.match(&.{.Comma})) break;
            }
        }
        
        _ = try self.consume(.RightParen, "Expect ')' after arguments.");
        
        const calleePtr = try self.allocator.create(Expr);
        calleePtr.* = callee;
        
        return Expr{ .Call = .{ .callee = calleePtr, .arguments = args.toOwnedSlice(self.allocator) catch return ParserError.OutOfMemory } };
    }

    fn primary(self: *Parser) ParserError!Expr {
        if (try self.match(&.{.False})) return Expr{ .Literal = .{ .Boolean = false } };
        if (try self.match(&.{.True})) return Expr{ .Literal = .{ .Boolean = true } };
        if (try self.match(&.{.Nil})) return Expr{ .Literal = .{ .Nil = {} } };
        if (try self.match(&.{.This})) {
            return Expr{ .This = .{ .keyword = self.previous.lexeme, .depth = -1, .slot = -1 } };
        }

        if (try self.match(&.{.Number})) {
             const num = std.fmt.parseFloat(f64, self.previous.lexeme) catch return ParserError.UnexpectedToken;
             return Expr{ .Literal = .{ .Number = num } };
        }

        if (try self.match(&.{.String})) {
            const s = self.previous.lexeme;
            return Expr{ .Literal = .{ .String = s[1..s.len-1] } };
        }

        if (try self.match(&.{.FString})) {
            const raw = self.previous.lexeme;
            // Remove f" (2 chars) and " (1 char)
            if (raw.len < 3) return Expr{ .Literal = .{ .String = "" } }; // Safety
            const content = raw[2..raw.len-1];
            
            var parts = std.ArrayList(Expr){};
            
            var i: usize = 0;
            while (i < content.len) {
                if (content[i] == '{') {
                    // Find closing '}'
                    var j = i + 1;
                    while (j < content.len) : (j += 1) {
                        if (content[j] == '}') break;
                    }
                    
                    if (j >= content.len) return ParserError.UnexpectedToken; 
                    
                    const snippet = content[i+1..j];
                    
                    // Parse snippet
                    var subLexer = Lexer.init(self.allocator, snippet);
                    var subParser = try Parser.init(self.allocator, &subLexer);
                    const expr = try subParser.expression();
                    
                    try parts.append(self.allocator, expr);
                    
                    i = j + 1;
                } else {
                    // Literal text
                    var j = i;
                    while (j < content.len and content[j] != '{') : (j += 1) {}
                    const text = content[i..j];
                    if (text.len > 0) {
                         try parts.append(self.allocator, Expr{ .Literal = .{ .String = text } });
                    }
                    i = j;
                }
            }
            return Expr{ .FString = .{ .parts = parts.toOwnedSlice(self.allocator) catch return ParserError.OutOfMemory } };
        }

        if (try self.match(&.{.Identifier})) {
            return Expr{ .Variable = .{ .name = self.previous.lexeme, .depth = -1, .slot = -1 } };
        }

        if (try self.match(&.{.LeftBracket})) {
            var elements = std.ArrayList(Expr){};
            if (!self.check(.RightBracket)) {
                while (true) {
                    const elem = try self.expression();
                    try elements.append(self.allocator, elem);
                    if (!try self.match(&.{.Comma})) break;
                }
            }
            _ = try self.consume(.RightBracket, "Expect ']' after list elements.");
            return Expr{ .ListLiteral = .{ .elements = elements.toOwnedSlice(self.allocator) catch return ParserError.OutOfMemory } };
        }

        if (try self.match(&.{.LeftParen})) {
            const expr = try self.expression();
            _ = try self.consume(.RightParen, "Expect ')' after expression.");
            const expr_ptr = try self.allocator.create(Expr);
            expr_ptr.* = expr;
            return Expr{ .Grouping = expr_ptr };
        }

        if (try self.match(&.{.LeftBrace})) {
            var keys = std.ArrayList(Expr){};
            var values = std.ArrayList(Expr){};
            if (!self.check(.RightBrace)) {
                while (true) {
                    const key = try self.expression();
                    _ = try self.consume(.Colon, "Expect ':' after key in dict.");
                    const val = try self.expression();
                    
                    try keys.append(self.allocator, key);
                    try values.append(self.allocator, val);
                    
                    if (!try self.match(&.{.Comma})) break;
                }
            }
            _ = try self.consume(.RightBrace, "Expect '}' after dict pairs.");
            return Expr{ .DictLiteral = .{ 
                .keys = keys.toOwnedSlice(self.allocator) catch return ParserError.OutOfMemory,
                .values = values.toOwnedSlice(self.allocator) catch return ParserError.OutOfMemory
            }};
        }

        std.debug.print("Expect expression. Found {any} at {d}:{d}\n", .{self.current.type, self.current.line, self.current.col});
        return ParserError.ExpectedExpression;
    }
};
