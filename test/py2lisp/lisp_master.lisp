(ql:quickload :mmap)
(ql:quickload :shasht)
(ql:quickload :kit)
(ql:quickload :wolffy)

(setf *print-pretty* nil)
(setf sb-impl::*default-external-format* :utf-8)
(setf sb-alien::*default-c-string-external-format* :utf-8)

(defparameter *mmap-file* nil)

(defun parse-data (data)
  (wo-pickle:loads (wo-io:make-binary-stream :initial-data data)))

(defun open-mmap (filepath)
  (multiple-value-bind (addr fd size) (mmap:mmap filepath)
    (setf *mmap-file* (list addr fd size))))

(defun close-mmap ()
  (destructuring-bind (addr fd size) *mmap-file*
    (mmap:munmap addr fd size)
    (setf *mmap-file* nil)))

(defun read-data ()
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))
  (destructuring-bind (addr fd size) *mmap-file*
    (declare (ignore fd size))
    (let* ((length-info (coerce (loop for i from 0 to 10
                                      for info = (cffi:mem-aref addr :unsigned-char i)
                                      while (/= info 0) collect info) 'vector))
           (index (array-total-size length-info))
           (data-len (parse-integer (octets-to-string (make-array index :element-type '(unsigned-byte 8) :initial-contents length-info))))
           (data (coerce (loop for i fixnum from (1+ index) to (+ index (the fixnum data-len)) collect (cffi:mem-aref addr :unsigned-char i)) 'vector)))
      (wo-pickle:loads (wo-io:make-binary-stream :initial-data data) :fast t))))

(defun read-data-thread ()
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))
  (destructuring-bind (addr fd size) *mmap-file*
    (declare (ignore fd size))
    (let* ((length-info (coerce (loop for i from 0 to 10
                                      for info = (cffi:mem-aref addr :unsigned-char i)
                                      while (/= info 0) collect info) 'vector))
           (index (array-total-size length-info))
           (data-len (parse-integer (octets-to-string (make-array index :element-type '(unsigned-byte 8) :initial-contents length-info))))
           (data (coerce (loop for i fixnum from (1+ index) to (+ index (the fixnum data-len)) collect (cffi:mem-aref addr :unsigned-char i)) 'vector)))
      (sb-thread:make-thread #'parse-data :arguments (list data)))))

(defun test () "aaaa")

