const std = @import("std");

pub const TokenType = enum {
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    Number,
    Colon,
    Semicolon,
    Identifier,
    Eof,
};

pub const Token = struct {
    type: TokenType,
    value: ?[]const u8,
    line: u64,
};

const LexerError = error{UnknownCharacter};

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    tokens: std.ArrayList(Token),
    source: []const u8,
    start: u64 = 0,
    current: u64 = 0,
    line: u64 = 1,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Lexer {
        return Lexer{
            .allocator = allocator,
            .tokens = std.ArrayList(Token).init(allocator),
            .source = source,
        };
    }

    pub fn scan(self: *Lexer) !void {
        while (!self.isAtEnd()) {
            const character = self.source[self.current];
            self.current += 1;

            if (std.ascii.isDigit(character) or character == '_') {
                self.start = self.current - 1;
                while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.source[self.current]) or self.source[self.current] == '_')) {
                    self.current += 1;
                }

                const token_value = self.source[self.start..self.current];
                if (std.ascii.isDigit(token_value[0])) {
                    try self.tokens.append(Token{ .type = TokenType.Number, .value = token_value, .line = self.line });
                } else {
                    try self.tokens.append(Token{ .type = TokenType.Identifier, .value = token_value, .line = self.line });
                }
                continue;
            }

            if (std.ascii.isAlphanumeric(character)) {
                self.start = self.current - 1;
                while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.source[self.current]) or self.source[self.current] == '_')) {
                    self.current += 1;
                }

                try self.tokens.append(Token{ .type = TokenType.Identifier, .value = self.source[self.start..self.current], .line = self.line });
                continue;
            }

            switch (character) {
                ' ' => {},
                '\t' => {},
                '\n' => {
                    self.line += 1;
                    self.start = self.current;
                },
                else => {
                    try self.tokens.append(Token{ .type = switch (character) {
                        '+' => TokenType.Plus,
                        '-' => TokenType.Minus,
                        '*' => TokenType.Star,
                        '/' => TokenType.Slash,
                        '=' => TokenType.Equal,
                        ':' => TokenType.Colon,
                        ';' => TokenType.Semicolon,
                        else => {
                            return LexerError.UnknownCharacter;
                        },
                    }, .value = null, .line = self.line });
                },
            }
        }

        try self.tokens.append(Token{ .type = TokenType.Eof, .value = null, .line = self.line });
    }

    pub fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    }
};
