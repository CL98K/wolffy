(defpackage wo-io/test
  (:use #:cl)
  (:import-from #:wo-io)
  (:export test))

(in-package #:wo-io/test)

(defun test ()
  (labels ((func0 ()
             (dotimes (i 1)
               (wo-io:make-binary-stream :initial-data #(49 49 49 49 49 49 49 49 49 49 49 49 49 49 49 49 49))))
           (func1 ()
             (dotimes (i 10000000)
               (let ((ins (wo-io:make-binary-stream :initial-data #(49 49 49 49 49 49 49 49 49 49 49 49 49 49 49 49 49))))
                 (wo-io:binary-stream-write ins 49)
                 (wo-io:binary-stream-write ins 50)
                 (wo-io:binary-stream-write ins 51)
                 (wo-io:binary-stream-write ins 52)
                 (wo-io:binary-stream-write ins 53)
                 (wo-io:binary-stream-write ins 54)
                 (wo-io:binary-stream-write ins 55)
                 (wo-io:binary-stream-write ins 56)
                 (wo-io:binary-stream-write ins 57)
                 (wo-io:binary-stream-write ins 58))))
           (func2 ()
             (dotimes (i 10000000)
               (let ((ins (wo-io:make-binary-stream))
                     (data (make-array 12 :element-type '(unsigned-byte 8) :initial-contents '(49 50 51 52 53 10 54 55 56 10 57 58))))
                 (wo-io:binary-stream-writes ins data)
                 (wo-io:binary-stream-writes ins data)
                 (wo-io:binary-stream-writes ins data)
                 (wo-io:binary-stream-writes ins data)
                 (wo-io:binary-stream-writes ins data)
                 (wo-io:binary-stream-writes ins data)
                 (wo-io:binary-stream-writes ins data)
                 (wo-io:binary-stream-writes ins data)
                 (wo-io:binary-stream-writes ins data)
                 (wo-io:binary-stream-writes ins data))))
           (func3 ()
             (dotimes (i 10000000)
               (let ((ins (wo-io:make-binary-stream :initial-data #(49 49 49 49 49 49 49 49 49 49 49 49 49 49 49 49 49))))
                 (dotimes (i 10)
                   (wo-io:binary-stream-read ins)))))
           (func4 ()
             (dotimes (i 10000000)
               (let ((ins (wo-io:make-binary-stream :initial-data #(49 49 49 49 49 49 49 49 49 49 49 49 49 49 49 49 49))))
                 (dotimes (i 10)
                   (wo-io:binary-stream-reads ins 8)))))
           (func5 ()
             (dotimes (i 10000000)
               (let ((ins (wo-io:make-binary-stream :initial-data #(1 12 3 14 15 10 1 2 3 4 5 10 6 7 8 9 10 10 1 2 3 4 5 6 7 8 9 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2))))
                 (dotimes (i 10)
                   (wo-io:binary-stream-read-line ins)))))
           (func6 ()
             (dotimes (i 10000000)
               (let ((ins (wo-io:make-binary-stream :initial-data #(1 12 3 14 15 10 1 2 3 4 5 10 6 7 8 9 10 10 1 2 3 4 5 6 7 8 9 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2)))
                     (buffer (make-array 8 :element-type '(unsigned-byte 8))))
                 (dotimes (i 10)
                   (wo-io:binary-stream-read-into ins buffer))))))
    
    (format t "Test MAKE ~%")
    (time (func0))    
    (format t "Test Write ~%")
    (time (func1))
    (format t "Test Writes ~%")
    (time (func2))
    (format t "Test Read ~%")
    (time (func3))
    (format t "Test Reads ~%")
    (time (func4))
    (format t "Test Line ~%")
    (time (func5))
    (format t "Test Into ~%")
    (time (func6))
    ))

