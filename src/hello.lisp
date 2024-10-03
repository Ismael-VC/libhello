(load "etc/quicklisp/setup.lisp")

(ql:quickload "cffi" :silent t)

(defpackage :hello
  (:use :cl :cffi))

(in-package :hello)

(cffi:load-foreign-library "lib/libhello.so")

(defcfun ("hello" hello) :void)

(hello)
