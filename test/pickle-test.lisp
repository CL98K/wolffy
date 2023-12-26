(defpackage wo-pickle/test
  (:use #:cl)
  (:import-from #:wo-pickle)
  (:export test))

(in-package #:wo-pickle/test)

(defun test ()
  (format t "1: ~A~%" (wo-pickle:dumps 1))
  (format t "-1: ~A~%" (wo-pickle:dumps -1))
  (format t "(1 2 \"a\" (2 3)): ~A~%" (wo-pickle:dumps '(1 2 "a" (2 3))))
  )
