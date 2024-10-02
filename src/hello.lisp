(ql:quickload "cffi")

(defpackage :hello
  (:use :cl))

(in-package :hello)

(defvar *libhello* (cffi:load-foreign-library "lib/libhello.so"))

(defcfun ("hello" c-hello) void)

(defun hello ()
  (c-hello))

(defun main ()
  (hello))

(main)
