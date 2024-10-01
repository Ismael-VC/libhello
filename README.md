# libhello

## Build

```bash
➜  libhello git:(main) make          
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
```

## Tree

```bash
➜  libhello git:(main) ✗ tree
.
├── bin
│   ├── dynamic_hello
│   ├── included_hello
│   ├── optimized_hello
│   ├── precompiled_hello
│   ├── shared_hello
│   └── static_hello
├── build
│   ├── hello.o
│   ├── libhello.h.gch
│   └── libhello.o
├── include
│   └── libhello.h
├── lib
│   ├── libhello.a
│   └── libhello.so
├── makefile
├── README.md
└── src
    ├── dynamic_hello.c
    ├── hello.c
    ├── included_hello.c
    └── libhello.c

6 directories, 18 files
```
