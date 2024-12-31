const std = @import("std");
const lexer = @import("lexer.zig");

pub const ParserError = error{
    ExpectedIdentifier,
    ExpectedEqual,
    ExpectedVar,
    ExpectedNumber,
    ExpectedOperator,
    UnexpectedToken,
    DivisionByZero,
};

pub const StatementType = enum {
    VarDeclaration,
};

pub const ExpressionType = enum {
    Number,
    BinaryOp,
};

pub const Expression = union(ExpressionType) {
    Number: f64,
    BinaryOp: struct {
        left: *Expression,
        operator: lexer.TokenType,
        right: *Expression,
    },

    pub fn evaluate(self: Expression) !f64 {
        return switch (self) {
            .Number => |value| value,
            .BinaryOp => |binary| {
                const left_value = try binary.left.evaluate();
                const right_value = try binary.right.evaluate();

                return switch (binary.operator) {
                    .Plus => left_value + right_value,
                    .Minus => left_value - right_value,
                    .Star => left_value * right_value,
                    .Slash => if (right_value == 0)
                        ParserError.DivisionByZero
                    else
                        left_value / right_value,
                    else => unreachable,
                };
            },
        };
    }

    pub fn print(self: Expression, writer: anytype, indent: u32) !void {
        switch (self) {
            .Number => |value| try writer.print("{d}", .{value}),
            .BinaryOp => |binary| {
                try writer.writeByte('(');
                try binary.left.print(writer, indent);
                try writer.print(" {s} ", .{switch (binary.operator) {
                    .Plus => "+",
                    .Minus => "-",
                    .Star => "*",
                    .Slash => "/",
                    else => unreachable,
                }});
                try binary.right.print(writer, indent);
                try writer.writeByte(')');
            },
        }
    }
};

pub const Statement = union(StatementType) {
    VarDeclaration: VarDeclarationStmt,

    pub fn print(self: Statement, writer: anytype) !void {
        switch (self) {
            .VarDeclaration => |stmt| try stmt.print(writer),
        }
    }

    pub fn evaluate(self: Statement) !f64 {
        return switch (self) {
            .VarDeclaration => |stmt| stmt.evaluate(),
        };
    }

    pub fn getIdentifier(self: Statement) []const u8 {
        return switch (self) {
            .VarDeclaration => |stmt| stmt.identifier,
        };
    }
};

pub const VarDeclarationStmt = struct {
    identifier: []const u8,
    expression: *Expression,

    pub fn print(self: VarDeclarationStmt, writer: anytype) !void {
        try writer.print("{s} = ", .{self.identifier});
        try self.expression.print(writer, 0);
    }

    pub fn evaluate(self: VarDeclarationStmt) !f64 {
        return self.expression.evaluate();
    }
};

pub const Variables = struct {
    map: std.StringHashMap(f64),

    pub fn init(allocator: std.mem.Allocator) Variables {
        return Variables{
            .map = std.StringHashMap(f64).init(allocator),
        };
    }

    pub fn set(self: *Variables, name: []const u8, value: f64) !void {
        try self.map.put(name, value);
    }

    pub fn get(self: Variables, name: []const u8) ?f64 {
        return self.map.get(name);
    }

    pub fn print(self: Variables, writer: anytype) !void {
        try writer.writeAll("\nVariables:\n");
        var it = self.map.iterator();
        while (it.next()) |entry| {
            try writer.print("  {s} = {d}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []lexer.Token,
    current: usize,
    variables: Variables,

    pub fn init(allocator: std.mem.Allocator, tokens: []lexer.Token) Parser {
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .variables = Variables.init(allocator),
        };
    }

    pub fn peek(self: *Parser) lexer.Token {
        return self.tokens[self.current];
    }

    pub fn advance(self: *Parser) lexer.Token {
        const token = self.peek();

        if (!self.isAtEnd()) {
            self.current += 1;
        }

        return token;
    }

    pub fn isAtEnd(self: *Parser) bool {
        return self.peek().type == lexer.TokenType.Eof;
    }

    pub fn match(self: *Parser, token_type: lexer.TokenType) bool {
        if (self.isAtEnd()) return false;

        if (self.peek().type == token_type) {
            _ = self.advance();
            return true;
        }

        return false;
    }

    pub fn parseStatement(self: *Parser) !Statement {
        const token = self.peek();

        return switch (token.type) {
            .Var => try self.parseVarDeclaration(),
            else => ParserError.UnexpectedToken,
        };
    }

    fn parseVarDeclaration(self: *Parser) !Statement {
        _ = self.advance();

        const identifier = self.peek();
        if (identifier.type != lexer.TokenType.Identifier) {
            return ParserError.ExpectedIdentifier;
        }
        _ = self.advance();

        if (!self.match(lexer.TokenType.Equal)) {
            return ParserError.ExpectedEqual;
        }

        const expr = try self.parseExpression();

        if (!self.match(lexer.TokenType.Semicolon)) {
            return ParserError.UnexpectedToken;
        }

        return Statement{
            .VarDeclaration = .{
                .identifier = identifier.value.?,
                .expression = expr,
            },
        };
    }

    pub fn parseExpression(self: *Parser) !*Expression {
        var left = try self.parseTerm();

        while (self.match(lexer.TokenType.Plus) or self.match(lexer.TokenType.Minus)) {
            const operator = self.tokens[self.current - 1].type;
            const right = try self.parseTerm();
            const binary_op = try self.allocator.create(Expression);
            binary_op.* = Expression{
                .BinaryOp = .{
                    .left = left,
                    .operator = operator,
                    .right = right,
                },
            };
            left = binary_op;
        }

        return left;
    }

    pub fn parseTerm(self: *Parser) !*Expression {
        var left = try self.parsePrimary();

        while (self.match(lexer.TokenType.Star) or self.match(lexer.TokenType.Slash)) {
            const operator = self.tokens[self.current - 1].type;
            const right = try self.parsePrimary();
            const binary_op = try self.allocator.create(Expression);

            binary_op.* = Expression{
                .BinaryOp = .{
                    .left = left,
                    .operator = operator,
                    .right = right,
                },
            };

            left = binary_op;
        }

        return left;
    }

    pub fn parsePrimary(self: *Parser) !*Expression {
        const token = self.peek();
        if (token.type != lexer.TokenType.Number) {
            return ParserError.ExpectedNumber;
        }

        _ = self.advance();

        const number = try self.allocator.create(Expression);
        const value = try std.fmt.parseFloat(f64, token.value.?);
        number.* = Expression{ .Number = value };

        return number;
    }
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const source: []const u8 = "var hello = 2 + 2 * 6;";

    var limonaca_lexer = lexer.Lexer.init(allocator, source);
    try limonaca_lexer.scan();

    var parser = Parser.init(allocator, limonaca_lexer.tokens.items);

    const stdout = std.io.getStdOut().writer();

    while (!parser.isAtEnd()) {
        const stmt = try parser.parseStatement();
        try stmt.print(stdout);

        const result = try stmt.evaluate();
        try stdout.print(" => {d}\n", .{result});

        try parser.variables.set(stmt.getIdentifier(), result);
    }

    try parser.variables.print(stdout);
}
