const std = @import("std");

extern fn hello() callconv(.C) void;

pub fn main() void {
    hello();
}
