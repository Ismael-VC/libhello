# libhello

## Build

```
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

```sh
➜  libhello git:(main) tree
.
├── bin
│   ├── dynamic_hello
│   ├── included_hello
│   ├── optimized_hello
│   ├── precompiled_hello
│   ├── shared_hello
│   └── static_hello
├── lib
│   ├── hello.o
│   ├── libhello.a
│   ├── libhello.h.gch
│   ├── libhello.o
│   └── libhello.so
├── makefile
└── src
    ├── dynamic_hello.c
    ├── hello.c
    ├── included_hello.c
    ├── libhello.c
    └── libhello.h

4 directories, 17 files
```
