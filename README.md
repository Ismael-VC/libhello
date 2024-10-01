# libhello

## Build

```bash
➜  libhello git:(main) ✗ make
Using shared library.
Hello, World!
Uisng static library.
Hello, World!
Using included library.
Hello, World!
Using precompiled header and code.
Hello, World!
Using dynamic linking.
Hello, World!
Using link time optimization.
Hello, World!
Using Fortran bindings.
Hello, World!
Using Julia bindings.
Hello, World!
Using Python bindings.
Hello, World!
Using R bindings.
Hello, World!
Using C++ bindings.
Hello, World!
Using LuaJIT bindings.
Hello, World!
Using Rust bindings.
Hello, World!
```

## Tree

```bash
➜  libhello git:(main) ✗ tree
.
├── include
│   └── libhello.h
├── makefile
├── README.md
└── src
    ├── dynamic_hello.c
    ├── hello.c
    ├── hello.cbl
    ├── hello.cpp
    ├── hello.f90
    ├── hello.jl
    ├── hello.lisp
    ├── hello.lua
    ├── hello.py
    ├── hello.R
    ├── hello.zig
    ├── included_hello.c
    ├── libhello.c
    └── rust_hello
        ├── build.rs
        ├── Cargo.lock
        ├── Cargo.toml
        └── src
            └── main.rs

5 directories, 20 files
```
