(defpackage #:wo-pickle
  (:documentation "wolffy pickle")
  (:use #:cl #:pack #:alexandria #:kit #:wo-io)
  (:export
   #:*highest-protocol*
   #:*default-protocal*
   #:py-load
   #:py-loads
   #:lisp-dump
   #:lisp-dumps))