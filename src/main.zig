const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const source: []const u8 = "var hello = 2 + 2 * 6 -> i32;";

    var limonaca_lexer = lexer.Lexer.init(allocator, source);
    try limonaca_lexer.scan();

    var limonaca_parser = parser.Parser.init(allocator, limonaca_lexer.tokens.items);

    const stdout = std.io.getStdOut().writer();

    while (!limonaca_parser.isAtEnd()) {
        const stmt = try limonaca_parser.parseStatement();
        try stmt.print(stdout);

        const result = try stmt.evaluate();
        try stdout.print(" => {d}\n", .{result});

        try limonaca_parser.variables.set(stmt.getIdentifier(), result, stmt.getType());
    }

    try limonaca_parser.variables.print(stdout);
}
