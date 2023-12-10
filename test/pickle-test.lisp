(defpackage wo-pickle/test
  (:use #:cl #:wo-pickle)
  (:export test))

(in-package #:wo-pickle/test)

(defun test ()
  (format t "~A~%" 111111111)
  ;(format t "1: ~A~%" (lisp-dumps 1))
  ;(format t "-1: ~A~%" (lisp-dumps -1))
  ;(format t "(1 2 \"a\" (2 3)): ~A~%" (lisp-dumps '(1 2 "a" (2 3))))
  )
