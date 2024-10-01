local ffi = require("ffi")

ffi.cdef[[
    void hello();
]]

local libhello = ffi.load("./lib/libhello.so")

libhello.hello()
