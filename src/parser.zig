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
        // Carrega o primeiro token
        try p.advance();
        return p;
    }

    fn advance(self: *Parser) !void {
        self.previous = self.current;
        while (true) {
            self.current = try self.lexer.nextToken();
            // std.debug.print("Token: {any} '{s}'\n", .{self.current.type, self.current.lexeme});
            if (self.current.type != .Error) break;
            // TODO: Reportar erro adequadamente, por enquanto ignora
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
        std.debug.print("Error at {d}:{d} - {s}\n", .{self.current.line, self.current.col, message});
        return ParserError.UnexpectedToken;
    }

    // --- Grammar Rules ---

    pub fn parse(self: *Parser) !std.ArrayList(Stmt) {
        var statements = std.ArrayList(Stmt){};
        while (!self.check(.Eof)) {
            // Ignora newlines soltas entre statements
            if (try self.match(&.{.Newline})) continue;
            
            const decl = try self.declaration();
            try statements.append(self.allocator, decl);
        }
        return statements;
    }

    fn declaration(self: *Parser) !Stmt {
        // Python doesn't have explicit 'var', but checking assignment logic
        // For simple MVP: identifier = expr
        if (self.current.type == .Identifier) {
            // Peek next token to see if it is '='
            // Note: Lexer/Parser structure needs peek capability or lookahead.
            // Simplified approach: treat as statement, verify inside statement()
        }
        
        return try self.statement();
    }

    fn statement(self: *Parser) ParserError!Stmt {
        if (try self.match(&.{.Print})) {
            return try self.printStatement();
        }
        
        // Verificar se é atribuição (identificador = ...)
        // Como o parser é recursivo, vamos simplificar:
        // Parse expression. Se for Variable, verifica se segue '='
        
        const expr = try self.expression();
        
        if (try self.match(&.{.Equal})) {
             const value = try self.expression();
             // Check if expr was a variable
             switch (expr) {
                 .Variable => |name| {
                     return Stmt{ .Var = .{ .name = name, .initializer = value } };
                 },
                 else => {
                     std.debug.print("Invalid assignment target.\n", .{});
                     return ParserError.UnexpectedToken;
                 }
             }
        }
        
        // Se não foi atribuição, é expression statement
        if (!self.check(.Eof)) {
             // Expect newline after statement
             // _ = try self.consume(.Newline, "Expect newline after statement.");
             // Relaxing for last line or REPL
             if (self.check(.Newline)) try self.advance();
        }
        
        return Stmt{ .Expression = .{ .expression = expr } };
    }

    fn printStatement(self: *Parser) !Stmt {
        _ = try self.consume(.LeftParen, "Expect '(' after print.");
        const value = try self.expression();
        _ = try self.consume(.RightParen, "Expect ')' after value.");
        // _ = try self.consume(.Newline, "Expect newline after print."); 
        if (self.check(.Newline)) try self.advance();
        return Stmt{ .Print = .{ .expression = value } };
    }

    fn expression(self: *Parser) !Expr {
        return self.equality();
    }

    fn equality(self: *Parser) !Expr {
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

    fn comparison(self: *Parser) !Expr {
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

    fn term(self: *Parser) !Expr {
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

    fn factor(self: *Parser) !Expr {
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
        return try self.primary();
    }

    fn primary(self: *Parser) !Expr {
        if (try self.match(&.{.False})) return Expr{ .Literal = .{ .Boolean = false } };
        if (try self.match(&.{.True})) return Expr{ .Literal = .{ .Boolean = true } };
        if (try self.match(&.{.Nil})) return Expr{ .Literal = .{ .Nil = {} } };

        if (try self.match(&.{.Number})) {
             const num = std.fmt.parseFloat(f64, self.previous.lexeme) catch return ParserError.UnexpectedToken;
             return Expr{ .Literal = .{ .Number = num } };
        }

        if (try self.match(&.{.String})) {
            // Remove aspas
            const s = self.previous.lexeme;
            return Expr{ .Literal = .{ .String = s[1..s.len-1] } };
        }

        if (try self.match(&.{.Identifier})) {
            return Expr{ .Variable = self.previous.lexeme };
        }

        if (try self.match(&.{.LeftParen})) {
            const expr = try self.expression();
            _ = try self.consume(.RightParen, "Expect ')' after expression.");
            const expr_ptr = try self.allocator.create(Expr);
            expr_ptr.* = expr;
            return Expr{ .Grouping = expr_ptr };
        }

        std.debug.print("Expect expression. Found {s}\n", .{self.current.lexeme});
        return ParserError.ExpectedExpression;
    }
};
