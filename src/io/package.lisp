(defpackage #:io
  (:documentation "wolffy io")
  (:nicknames :wo-io :wol-io)
  (:use #:cl)
  (:import-from #:sb-gray)
  (:import-from #:fast-io)
  (:import-from #:alexandria)
  (:export
   #:binary-stream
   #:make-binary-stream
   #:binary-stream-close
   #:binary-stream-info
   #:binary-stream-memery-view
   #:binary-stream-file-position
   #:binary-stream-upgrade-space
   #:binary-stream-write
   #:binary-stream-writes
   #:binary-stream-read
   #:binary-stream-reads
   #:binary-stream-read-line
   #:binary-stream-read-into
   #:binary-stream-read-sequence
   #:file-stream-to-binary-stream))
