(ql:quickload :cffi)
(ql:quickload :mmap)
(ql:quickload :shasht)
(ql:quickload :kit)
(ql:quickload :wolffy)

(setf *print-pretty* nil)
(setf sb-impl::*default-external-format* :utf-8)
(setf sb-alien::*default-c-string-external-format* :utf-8)

(defparameter *mmap-file* nil)

(defun open-space (filepath)
  (multiple-value-bind (addr fd size) (mmap:mmap filepath :open '(:read :write) :protection '(:read :write) :mmap '(:shared))
    (setf *mmap-file* (list addr fd size))))

(defun close-space ()
  (destructuring-bind (addr fd size) *mmap-file*
    (mmap:munmap addr fd size)
    (setf *mmap-file* nil)))

(defun read-data (addr)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))
  
  (let* ((length-info (coerce (loop for i from 0 to 10
                                    for info = (cffi:mem-aref addr :unsigned-char i)
                                    while (/= info 0) collect info) 'vector))
         (index (array-total-size length-info))
         (data-len (parse-integer (octets-to-string (make-array index :element-type '(unsigned-byte 8) :initial-contents length-info))))
         (data (coerce (loop for i fixnum from (1+ index) to (+ index (the fixnum data-len)) collect (cffi:mem-aref addr :unsigned-char i)) 'vector)))
    
    (wo-pickle:loads (wo-io:make-binary-stream :initial-data data) :fast t)))

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

(defun pycall (func &key (offset 0))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type function func) (type fixnum offset))
  
  (destructuring-bind (addr fd size) *mmap-file*
    (declare (ignore fd size))

    (destructuring-bind (args kwargs) (read-data addr)
      (write-data addr (apply func args)))))

