(defpackage #:pickle
  (:documentation "wolffy pickle")
  (:nicknames :wo-pickle :wol-pickle :wo-pkl :wol-pkl)
  (:use #:cl)
  (:import-from #:uiop)
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
