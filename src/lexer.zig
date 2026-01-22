const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

pub const Lexer = struct {
    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,
    line_start: usize = 0,
    
    indent_stack: std.ArrayList(usize),
    pending_dedents: usize = 0, // Unused now
    at_line_start: bool = true,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Lexer {
        var stack = std.ArrayList(usize){};
        stack.append(allocator, 0) catch unreachable; 
        return Lexer{
            .source = source,
            .indent_stack = stack,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.indent_stack.deinit(self.allocator);
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Lexer) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn peekNext(self: *Lexer) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn makeToken(self: *Lexer, type_: TokenType) Token {
        return Token{
            .type = type_,
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
            .col = self.start - self.line_start,
        };
    }

    fn errorToken(self: *Lexer, message: []const u8) Token {
        return Token{
            .type = .Error,
            .lexeme = message,
            .line = self.line,
            .col = self.start - self.line_start,
        };
    }

    fn skipWhitespace(self: *Lexer) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ' => {
                    _ = self.advance();
                },
                '\r' => {
                    _ = self.advance();
                },
                '\t' => {
                    _ = self.advance();
                },
                '#' => {
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                },
                else => return,
            }
        }
    }
    
    fn handleIndentation(self: *Lexer) !?Token {
        self.start = self.current;
        
        var spaces: usize = 0;
        var idx = self.current;
        while (idx < self.source.len and self.source[idx] == ' ') : (idx += 1) {
            spaces += 1;
        }

        if (idx >= self.source.len) return null;
        if (self.source[idx] == '\n' or self.source[idx] == '#') return null;

        const current_indent = self.indent_stack.getLast();

        if (spaces > current_indent) {
            try self.indent_stack.append(self.allocator, spaces);
            self.current += spaces;
            return self.makeToken(.Indent);
        } else if (spaces < current_indent) {
            _ = self.indent_stack.pop();
            if (spaces > self.indent_stack.getLast()) {
                 return self.errorToken("Inconsistent indentation");
            }
            return self.makeToken(.Dedent);
        }
        
        self.current += spaces;
        return null;
    }

    pub fn nextToken(self: *Lexer) !Token {
        if (self.at_line_start) {
            if (try self.handleIndentation()) |token| {
                if (token.type == .Indent) self.at_line_start = false;
                return token;
            }
            self.at_line_start = false;
        }

        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(.Eof);

        const c = self.advance();

        if (std.ascii.isAlphabetic(c) or c == '_') return self.identifier();
        if (std.ascii.isDigit(c)) return self.number();

        switch (c) {
            '(' => return self.makeToken(.LeftParen),
            ')' => return self.makeToken(.RightParen),
            '{' => return self.makeToken(.LeftBrace),
            '}' => return self.makeToken(.RightBrace),
            ',' => return self.makeToken(.Comma),
            '.' => return self.makeToken(.Dot),
            '-' => return self.makeToken(.Minus),
            '+' => return self.makeToken(.Plus),
            ';' => return self.makeToken(.Semicolon),
            '*' => return self.makeToken(.Star),
            '/' => return self.makeToken(.Slash),
            ':' => return self.makeToken(.Colon),
            '!' => return self.makeToken(if (self.match('=')) .BangEqual else .Bang),
            '=' => return self.makeToken(if (self.match('=')) .EqualEqual else .Equal),
            '<' => return self.makeToken(if (self.match('=')) .LessEqual else .Less),
            '>' => return self.makeToken(if (self.match('=')) .GreaterEqual else .Greater),
            '"' => return self.string(),
            '\n' => {
                const token = self.makeToken(.Newline);
                self.line += 1;
                self.line_start = self.current;
                self.at_line_start = true;
                return token;
            },
            else => return self.errorToken("Unexpected character."),
        }
    }

    fn identifier(self: *Lexer) Token {
        while (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_') {
            _ = self.advance();
        }
        const text = self.source[self.start..self.current];
        
        const type_ = if (std.mem.eql(u8, text, "and")) TokenType.And
        else if (std.mem.eql(u8, text, "class")) TokenType.Class
        else if (std.mem.eql(u8, text, "else")) TokenType.Else
        else if (std.mem.eql(u8, text, "false")) TokenType.False
        else if (std.mem.eql(u8, text, "for")) TokenType.For
        else if (std.mem.eql(u8, text, "def")) TokenType.Fun
        else if (std.mem.eql(u8, text, "if")) TokenType.If
        else if (std.mem.eql(u8, text, "None")) TokenType.Nil
        else if (std.mem.eql(u8, text, "or")) TokenType.Or
        else if (std.mem.eql(u8, text, "print")) TokenType.Print
        else if (std.mem.eql(u8, text, "return")) TokenType.Return
        else if (std.mem.eql(u8, text, "super")) TokenType.Super
        else if (std.mem.eql(u8, text, "True")) TokenType.True
        else if (std.mem.eql(u8, text, "var")) TokenType.Var 
        else if (std.mem.eql(u8, text, "while")) TokenType.While
        else TokenType.Identifier;

        return self.makeToken(type_);
    }

    fn number(self: *Lexer) Token {
        while (std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            _ = self.advance(); 
            while (std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        return self.makeToken(.Number);
    }

    fn string(self: *Lexer) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) return self.errorToken("Unterminated string.");

        _ = self.advance(); 
        return self.makeToken(.String);
    }
};