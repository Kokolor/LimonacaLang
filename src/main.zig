const std = @import("std");

pub const TokenType = enum { Plus, Minus, Star, Slash, Number, Eof };

pub const Token = struct { type: TokenType, value: ?[]const u8, line: u64 };

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    tokens: std.ArrayList(Token),
    source: []const u8,
    start: u64 = 0,
    current: u64 = 0,
    line: u64 = 1,

    pub fn init(self: *Lexer, allocator: std.mem.Allocator, source: []const u8) !void {
        self.allocator = allocator;
        self.tokens = std.ArrayList(Token).init(allocator);
        self.source = source;
    }

    pub fn scan(self: *Lexer) !void {
        while (!self.isAtEnd()) {
            const character = self.source[self.current];
            self.current += 1;

            if (std.ascii.isDigit(character)) {
                self.start = self.current - 1;
                while (!self.isAtEnd() and std.ascii.isDigit(self.source[self.current])) {
                    self.current += 1;
                }

                try self.tokens.append(Token{ .type = TokenType.Number, .value = self.source[self.start..self.current], .line = self.line });
                continue;
            }

            switch (character) {
                ' ' => {},
                '\t' => {},
                '\n' => {
                    self.line += 1;
                    self.start = self.current;
                },
                '+' => try self.tokens.append(Token{ .type = TokenType.Plus, .value = null, .line = self.line }),
                '-' => try self.tokens.append(Token{ .type = TokenType.Minus, .value = null, .line = self.line }),
                '*' => try self.tokens.append(Token{ .type = TokenType.Star, .value = null, .line = self.line }),
                '/' => try self.tokens.append(Token{ .type = TokenType.Slash, .value = null, .line = self.line }),
                else => {},
            }
        }

        try self.tokens.append(Token{ .type = TokenType.Eof, .value = null, .line = self.line });
    }

    pub fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    }
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var lexer = Lexer{
        .allocator = allocator,
        .tokens = std.ArrayList(Token).init(allocator),
        .source = "74 + 2",
    };

    try lexer.init(allocator, lexer.source);
    try lexer.scan();

    for (lexer.tokens.items) |token| {
        if (token.value) |value| {
            std.debug.print("Token: {}, Value: {s}, Line: {}\n", .{ token.type, value, token.line });
        } else {
            std.debug.print("Token: {}, Line: {}\n", .{ token.type, token.line });
        }
    }
}