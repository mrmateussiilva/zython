const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Interpreter = @import("interpreter.zig").Interpreter;
const Resolver = @import("resolver.zig").Resolver;
const Compiler = @import("compiler.zig").Compiler;
const VM = @import("vm.zig").VM;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        try runRepl(allocator);
    } else if (args.len == 2) {
        try runFile(allocator, args[1]);
    } else {
        std.debug.print("Usage: zython [script]\n", .{});
        std.process.exit(64);
    }
}

fn runRepl(allocator: std.mem.Allocator) !void {
    var in_buf: [4096]u8 = undefined;
    var out_buf: [4096]u8 = undefined;

    const stdin_struct = std.fs.File.stdin().reader(&in_buf);
    var stdout_struct = std.fs.File.stdout().writer(&out_buf);
    
    var stdin = stdin_struct.interface;
    var stdout = stdout_struct.interface;

    try stdout.print("Zython 0.1.0\n", .{});
    if (@hasDecl(@TypeOf(stdout_struct), "flush")) try stdout_struct.flush();
    
    // Arena allocator for per-line processing
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var buffer: [1024]u8 = undefined;

    while (true) {
        try stdout.print(">>> ", .{});
        if (@hasDecl(@TypeOf(stdout_struct), "flush")) try stdout_struct.flush();

        const maybe_input = try readLine(&stdin, &buffer);
        
        if (maybe_input) |input| {
            if (std.mem.eql(u8, input, "exit()")) break;
            
            // Reset arena for new line
            _ = arena.reset(.retain_capacity);
            const line_allocator = arena.allocator();

            try run(line_allocator, input);
        } else {
            break; // EOF
        }
    }
}

fn readLine(reader: anytype, buffer: []u8) !?[]const u8 {
    var index: usize = 0;
    while (index < buffer.len) {
        var byte: [1]u8 = undefined;
        // readSliceShort reads at least 1 byte if available
        const n = try reader.readSliceShort(&byte);
        if (n == 0) return if (index == 0) null else buffer[0..index];
        if (byte[0] == '\n') return buffer[0..index];
        buffer[index] = byte[0];
        index += 1;
    }
    return buffer[0..index];
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const file_size = (try file.stat()).size;
    const source = try allocator.alloc(u8, file_size);
    defer allocator.free(source);

    _ = try file.readAll(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    
    try run(arena.allocator(), source);
}

fn run(allocator: std.mem.Allocator, source: []const u8) !void {
    var lexer = Lexer.init(allocator, source);
    defer lexer.deinit();

    var parser = try Parser.init(allocator, &lexer);
    // Parser holds refs to tokens which hold refs to source.
    
    const statements = parser.parse() catch {
        std.debug.print("Parse Error.\n", .{});
        return;
    };

    var resolver = Resolver.init(allocator);
    defer resolver.deinit();
    resolver.resolve(statements.items) catch {
        std.debug.print("Resolver Error.\n", .{});
        return;
    };

    var compiler = Compiler.init(allocator, 0);
    const bc_func = compiler.compileScript(statements.items) catch {
        std.debug.print("Bytecode indisponivel, usando tree-walker.\n", .{});
        var interpreter = try Interpreter.init(allocator);
        defer interpreter.deinit();
        interpreter.interpret(statements.items) catch {
            std.debug.print("Runtime Error.\n", .{});
        };
        return;
    };

    var vm = try VM.init(allocator);
    defer vm.deinit();
    vm.interpret(bc_func) catch {
        std.debug.print("Runtime Error.\n", .{});
    };
}
