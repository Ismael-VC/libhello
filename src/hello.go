package main

// #cgo CFLAGS: -I ../include
// #cgo LDFLAGS: -L ../lib -l hello
// #include "libhello.h"
import "C"

func main() {
	C.hello()
}
