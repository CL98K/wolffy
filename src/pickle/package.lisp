(defpackage #:wo-pickle
  (:documentation "wolffy pickle")
  (:use #:cl)
  (:import-from #:pack)
  (:import-from #:alexandria)
  (:import-from #:kit
                #:integer-to-bytes
                #:bytes-to-integer)
  (:import-from #:wo-io)
  (:shadow #:load)
  (:export
   #:*highest-protocol*
   #:*default-protocol*
   #:load
   #:loads
   #:dump
   #:dumps))
