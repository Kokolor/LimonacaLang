const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

const Variable = struct {
    offset: i32,
    type_token: lexer.TokenType,
};

pub const CodeGenerator = struct {
    writer: std.fs.File.Writer,
    variables: std.StringHashMap(Variable),
    current_stack_offset: i32,
    label_counter: usize,

    pub fn init(writer: std.fs.File.Writer) CodeGenerator {
        return CodeGenerator{
            .writer = writer,
            .variables = std.StringHashMap(Variable).init(std.heap.page_allocator),
            .current_stack_offset = 0,
            .label_counter = 0,
        };
    }

    fn generateLabel(self: *CodeGenerator) ![]const u8 {
        const label = try std.fmt.allocPrint(
            std.heap.page_allocator,
            ".L{d}",
            .{self.label_counter},
        );
        self.label_counter += 1;
        return label;
    }

    pub fn generateHeader(self: *CodeGenerator) !void {
        try self.writer.writeAll(
            \\section .text
            \\global _start
            \\
            \\_start:
            \\    push rbp
            \\    mov rbp, rsp
            \\
        );
    }

    pub fn generateExpression(self: *CodeGenerator, expr: parser.Expression) !void {
        switch (expr) {
            .Number => |value| {
                try self.writer.print("    mov rax, {d}\n", .{@as(i64, @intFromFloat(value))});
            },
            .BinaryOp => |binary| {
                try self.generateExpression(binary.right.*);
                try self.writer.writeAll("    push rax\n");

                try self.generateExpression(binary.left.*);

                try self.writer.writeAll("    pop rbx\n");

                switch (binary.operator) {
                    .Plus => try self.writer.writeAll("    add rax, rbx\n"),
                    .Minus => try self.writer.writeAll("    sub rax, rbx\n"),
                    .Star => try self.writer.writeAll("    imul rax, rbx\n"),
                    .Slash => {
                        try self.writer.writeAll(
                            \\    xor rdx, rdx
                            \\    div rbx
                            \\
                        );
                    },
                    else => unreachable,
                }
            },
        }
    }

    pub fn generateVarDeclaration(self: *CodeGenerator, stmt: parser.VarDeclarationStmt) !void {
        try self.generateExpression(stmt.expression.*);

        const var_size: i32 = switch (stmt.type) {
            .i16 => 2,
            .i32 => 4,
            else => unreachable,
        };

        self.current_stack_offset -= var_size;
        try self.variables.put(stmt.identifier, .{
            .offset = self.current_stack_offset,
            .type_token = stmt.type,
        });

        try self.writer.print("    mov [rbp{d}], ", .{self.current_stack_offset});
        switch (stmt.type) {
            .i16 => try self.writer.writeAll("ax\n"),
            .i32 => try self.writer.writeAll("eax\n"),
            else => unreachable,
        }
    }

    pub fn generateExit(self: *CodeGenerator) !void {
        try self.writer.writeAll(
            \\    mov rsp, rbp
            \\    pop rbp
            \\
        );

        try self.writer.writeAll(
            \\    mov rax, 60
            \\    xor rdi, rdi
            \\    syscall
            \\
        );
    }
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const source: []const u8 = "var hello = 2 + 2 * 6 -> i32;\n var test = 4 * 2 -> i32;";

    const asm_file = try std.fs.cwd().createFile("output.s", .{});
    defer asm_file.close();

    var limonaca_lexer = lexer.Lexer.init(allocator, source);
    try limonaca_lexer.scan();

    var limonaca_parser = parser.Parser.init(allocator, limonaca_lexer.tokens.items);
    var code_generator = CodeGenerator.init(asm_file.writer());

    try code_generator.generateHeader();

    while (!limonaca_parser.isAtEnd()) {
        const stmt = try limonaca_parser.parseStatement();
        const result = try stmt.evaluate();
        try limonaca_parser.variables.set(stmt.getIdentifier(), result, stmt.getType());

        switch (stmt) {
            .VarDeclaration => |var_decl| try code_generator.generateVarDeclaration(var_decl),
        }
    }

    try code_generator.generateExit();
}
