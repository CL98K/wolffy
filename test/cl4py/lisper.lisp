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

(declaim (ftype (function (system-area-pointer fixnum fixnum) t) %read-data) (inline %read-data))
(defun %read-data (addr offset size)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type system-area-pointer addr) (type fixnum offset size))
  
  (let* ((data (coerce (loop for i fixnum from (1+ offset) to (+ offset size) collect (cffi:mem-aref addr :unsigned-char i)) 'vector)))
    (wo-pickle:loads (wo-io:make-binary-stream :initial-data data) :fast t)))

(defun read-data (addr ipoint)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type system-area-pointer addr) (type fixnum ipoint))
  
  (let* ((length-info (coerce (loop for i from (1+ ipoint) to (+ 10 ipoint)
                                    for info = (cffi:mem-aref addr :unsigned-char i)
                                    while (/= info 0) collect info) 'vector))
         (offset (array-total-size length-info))
         (data-len (parse-integer (octets-to-string (make-array offset :element-type '(unsigned-byte 8) :initial-contents length-info)))))
    
    (%read-data addr (+ ipoint offset 1) data-len)))

(defun write-data (addr offset limit data)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type system-area-pointer addr) (type fixnum offset limit))

  (print (wo-pkl:dumps data))
  
  ;; (multiple-value-bind (val size) (wo-pkl:dumps data)
  ;;   (let ((i (1+ offset)))
  ;;     (declare (type fixnum i))
  ;;     (loop for v across (sb-ext:string-to-octets (write-to-string size) :external-format :utf-8)
  ;;           do
  ;;           (setf (cffi:mem-aref addr :unsigned-char i) v)
  ;;           (incf i))
      
  ;;     (setf (cffi:mem-aref addr :unsigned-char i) 0)
      
  ;;     (loop for v across val
  ;;           do
  ;;           (incf i)
  ;;           (setf (cffi:mem-aref addr :unsigned-char i) v))
  ;;     (setf (cffi:mem-aref addr :unsigned-char offset) 48)))
  )

(defun pycall (func &key (offset 0) (size 0) (ipoint 0) (limit 0))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type function func) (type fixnum offset size ipoint limit))

  (destructuring-bind (addr fd space-size) *mmap-file*
    (declare (ignore fd space-size))
    
    (destructuring-bind (args kwargs) (%read-data addr (+ ipoint offset) size)
      (write-data addr ipoint limit (apply func args))))
  
  (return-from pycall nil))

;;;测试接口
(defun test-interface (&rest args) args)

;;;测试服务
(defun main ()
  (open-space "/mnt/d/Worker/Lisp/tools/wolffy/test/cl4py/_smp/1231.sm")
  (pycall #'test-interface :offset 5 :size 3525 :ipoint 0 :limit 8191)
  ;; (time (loop repeat 10000 do (pycall #'test-interface :offset 5 :size 3525 :ipoint 0 :limit 8191)))
  
  (close-space))

(main)
(quit)
