const std = @import("std");
const lexer = @import("lexer.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var limonaca_lexer = lexer.Lexer{
        .allocator = allocator,
        .tokens = std.ArrayList(lexer.Token).init(allocator),
        .source = "_hello = 74 + 2\n42test =        63 - 7",
    };

    try limonaca_lexer.init(allocator, limonaca_lexer.source);
    try limonaca_lexer.scan();

    for (limonaca_lexer.tokens.items) |token| {
        if (token.value) |value| {
            std.debug.print("Token: {}, Value: {s}, Line: {}\n", .{ token.type, value, token.line });
        } else {
            std.debug.print("Token: {}, Line: {}\n", .{ token.type, token.line });
        }
    }
}
