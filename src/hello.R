dyn.load("lib/libhello.so")

hello <- function() {
  invisible(.C("hello"))
}

hello()

dyn.unload("lib/libhello.so")
