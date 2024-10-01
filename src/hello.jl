using Libdl: dlopen, dlsym, dlclose

libhello = dlopen("lib/libhello.so")
hello_c = dlsym(libhello, "hello")

hello() = @ccall $hello_c()::Cvoid

if abspath(PROGRAM_FILE) == @__FILE__
    hello()
    dlclose(libhello)
end
