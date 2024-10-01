require 'ffi'

module HelloLib
  extend FFI::Library
  ffi_lib './lib/libhello.so'

  attach_function :hello, [], :void
end

HelloLib.hello
