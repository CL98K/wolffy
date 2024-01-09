(ql:quickload :mmap)
(ql:quickload :shasht)
(ql:quickload :kit)
(ql:quickload :wolffy)

(setf *print-pretty* nil)
(setf sb-impl::*default-external-format* :utf-8)
(setf sb-alien::*default-c-string-external-format* :utf-8)

(defparameter *mmap-file* nil)

(defun open-mmap (filepath)
  (multiple-value-bind (addr fd size) (mmap:mmap filepath :open '(:read :write) :protection '(:read :write) :mmap '(:shared))
    (setf *mmap-file* (list addr fd size))))

(defun close-mmap ()
  (destructuring-bind (addr fd size) *mmap-file*
    (mmap:munmap addr fd size)
    (setf *mmap-file* nil)))

(defun write-data (addr data)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))
  
  (multiple-value-bind (val size) (wo-pkl:dumps data)
    (let ((i 0))
      (declare (type fixnum i))
      (loop for v across (sb-ext:string-to-octets (write-to-string size) :external-format :utf-8)
            do
            (setf (cffi:mem-aref addr :unsigned-char i) v)
            (incf i))
      
      (setf (cffi:mem-aref addr :unsigned-char i) 0)
      
      (loop for v across val
            do
            (incf i)
            (setf (cffi:mem-aref addr :unsigned-char i) v)))))

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
      
      (write-data addr (wo-pickle:loads (wo-io:make-binary-stream :initial-data data) :fast t)))))

;; (defun read-data-thread ()
;;   (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))
;;   (destructuring-bind (addr fd size) *mmap-file*
;;     (declare (ignore fd size))
;;     (let* ((length-info (coerce (loop for i from 0 to 10
;;                                       for info = (cffi:mem-aref addr :unsigned-char i)
;;                                       while (/= info 0) collect info) 'vector))
;;            (index (array-total-size length-info))
;;            (data-len (parse-integer (octets-to-string (make-array index :element-type '(unsigned-byte 8) :initial-contents length-info))))
;;            (data (coerce (loop for i fixnum from (1+ index) to (+ index (the fixnum data-len)) collect (cffi:mem-aref addr :unsigned-char i)) 'vector)))
;;       (sb-thread:make-thread #'parse-data :arguments (list data)))))
