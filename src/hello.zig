const std = @import("std");

extern "c" fn hello() void;

pub fn main() void {
    hello();
}
