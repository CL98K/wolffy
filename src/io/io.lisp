(in-package #:io)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(declaim (inline +newline+))
(defconstant +newline+ (char-code #\newline))

(defstruct binary
  (stream nil :type fast-io:octet-vector)
  (size 0 :type fixnum)
  (restream-size 1.5 :type single-float)
  (w-pointer 0 :type fixnum)
  (r-pointer 0 :type fixnum)
  (s-pointer 0 :type fixnum)
  (separator nil))

(deftype binary-stream () `binary)

(defmethod print-object ((object binary) stream)
  (format t "#<IO:BINARY-STREAM {~A}" (sb-kernel:get-lisp-obj-address object)))

(defun make-binary-stream (&key (initial-data nil initial-data-p) (initial-size 128 initial-size-p) (upgrade-p t) (reuse-p nil))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type (or simple-vector fast-io:octet-vector) initial-data) (type fixnum initial-size) (type boolean upgrade-p reuse-p))
  
  (let* ((instance nil)
         (initial-size (if initial-data-p (if initial-size-p initial-size (array-total-size initial-data)) initial-size))
         (stream (if reuse-p initial-data (fast-io:make-octet-vector initial-size))))
    (setf instance (make-binary :stream stream :size initial-size))
    (if initial-data-p (binary-stream-writes instance (coerce initial-data 'fast-io:octet-vector) :upgrade-p upgrade-p :reuse-p reuse-p))
    instance))

(defun binary-stream-close (instance)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance))

  (setf (binary-stream instance) (fast-io:make-octet-vector 0))
  t)

(defun binary-stream-info (instance)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance))

  (format t
          "#(:STREAN ~A~% :SIZE ~A~% :W-POINTER ~A~% :R-POINTER ~A~% :S-POINTER ~A~% :SEPARATOR ~A~%)"
          (binary-stream instance)
          (binary-size instance)
          (binary-w-pointer instance)
          (binary-r-pointer instance)
          (binary-s-pointer instance)
          (binary-separator instance)))

(defun binary-stream-memery-view (instance)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance) (type boolean show))
  
  (let* ((stream (binary-stream instance))
         (w-pointer (binary-w-pointer instance)))
    (declare (type fast-io:octet-vector stream) (type fixnum w-pointer))

    (values stream w-pointer)))

(defun binary-stream-file-position (instance &optional position-spec)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance) (type (or fixnum null) position-spec))

  (let* ((sep (binary-separator instance)))
    (declare (type list sep))
    
    (when position-spec
      (setf (binary-r-pointer instance) position-spec)
      (setf (binary-s-pointer instance) (loop for i fixnum downfrom (1- (length sep)) to 0
                                              for j fixnum = (nth i sep)
                                              for x fixnum from 0
                                              while (< j position-spec) finally (return x))))
    (binary-r-pointer instance)))

(defun binary-stream-upgrade-space (instance &optional (size 0))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance) (type fixnum size))
  
  (let* ((stream (binary-stream instance))
         (stream-size (binary-size instance))
         (restream-size (binary-restream-size instance))
         (w-pointer (binary-w-pointer instance))
         (upgrade-size (if (= size 0) (floor (* stream-size restream-size)) size))
         (buffer (fast-io:make-octet-vector upgrade-size)))
    (declare (type fast-io:octet-vector stream buffer) (type fixnum stream-size upgrade-size))
        
    (when (> w-pointer 0)
      (loop for i fixnum from 0 below stream-size do (setf (aref buffer i) (aref stream i))))

    (setf (binary-stream instance) buffer)
    (setf (binary-size instance) (1- upgrade-size))))

(defun binary-stream-write (instance integer)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance) (type fixnum integer))

  (let* ((stream (binary-stream instance))
         (stream-size (binary-size instance))
         (w-pointer (binary-w-pointer instance))
         (sep (binary-separator instance)))
    (declare (type fast-io:octet-vector stream) (type fixnum stream-size w-pointer) (type list sep))
    
    (if (>= w-pointer stream-size) (binary-stream-upgrade-space instance))
    (if (eq integer +newline+) (push w-pointer sep))

    (setf (aref stream w-pointer) integer)
    (incf (binary-w-pointer instance))))

(defun binary-stream-writes (instance integers &key (start 0) (end 0 end-p) (upgrade-p t) (reuse-p nil))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance) (type fast-io:octet-vector integers) (type fixnum start end) (type boolean upgrade-p reuse-p))
  
  (let* ((stream (binary-stream instance))
         (stream-size (binary-size instance))
         (w-pointer (binary-w-pointer instance))
         (sep (binary-separator instance))
         (data-size (if end-p end (array-total-size integers))))
    (declare (type fast-io:octet-vector stream) (type fixnum data-size stream-size w-pointer) (type list sep))
    
    (when (and upgrade-p (< (- (the fixnum (- stream-size w-pointer)) data-size) 0))
      (binary-stream-upgrade-space instance (if end-p (the fixnum (+ stream-size data-size)) 0))
      (setf stream (binary-stream instance)))

    (loop for i fixnum from start below data-size
          for j fixnum from w-pointer
          for integer fixnum = (aref integers i)
          do
          (if (not reuse-p) (setf (aref stream j) integer))
          (if (eq integer +newline+) (push j sep)))
    
    (incf (binary-w-pointer instance) data-size)))

(defun binary-stream-read (instance)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance))
  
  (let* ((stream (binary-stream instance))
         (w-pointer (binary-w-pointer instance))
         (r-pointer (binary-r-pointer instance)))
    (declare (type fast-io:octet-vector stream) (type fixnum w-pointer r-pointer))
    
    (if (>= r-pointer w-pointer) (return-from binary-stream-read nil))
    (prog1 (aref stream r-pointer)
      (incf (binary-r-pointer instance)))))

(defun binary-stream-reads (instance &optional (n 1))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance) (type fixnum n))
  
  (let* ((stream (binary-stream instance))
         (w-pointer (binary-w-pointer instance))
         (r-pointer (binary-r-pointer instance))
         (diff (- w-pointer r-pointer))
         (n (if (> diff n) n diff))
         (buffer (fast-io:make-octet-vector n)))
    (declare (type fast-io:octet-vector stream buffer) (type fixnum w-pointer r-pointer diff n))

    (loop for i fixnum from r-pointer
          for j fixnum from 0 to (1- n)
          do (setf (aref buffer j) (aref stream i)))

    (incf (binary-r-pointer instance) n)
    (values buffer n)))

(defun binary-stream-read-line (instance)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance))
  
  (let* ((stream (binary-stream instance))
         (w-pointer (binary-w-pointer instance))
         (r-pointer (binary-r-pointer instance))
         (s-pointer (binary-s-pointer instance))
         (sep (binary-separator instance))
         (sep-size (length sep))
         (index (if sep (nth (- sep-size s-pointer 1) sep) w-pointer))
         (n (if (/= index w-pointer) (if (> index r-pointer) (1+ (- index r-pointer)) 0) (- w-pointer r-pointer)))
         (buffer (fast-io:make-octet-vector n)))
    (declare (type fast-io:octet-vector stream buffer) (type list sep) (type fixnum w-pointer r-pointer sep-size index n))

    (when (and (= n 0) (> w-pointer r-pointer))
      (setf n (- w-pointer r-pointer))
      (setf buffer (fast-io:make-octet-vector n)))
    
    (prog1 buffer
      (loop for i fixnum from r-pointer
            for j fixnum from 0 to (1- n)
            do (setf (aref buffer j) (aref stream i)))
      (incf (binary-r-pointer instance) n)
      (if (< (1+ s-pointer) sep-size) (incf (binary-s-pointer instance))))))

(defun binary-stream-read-into (instance buffer)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance) (type fast-io:octet-vector buffer))
  
  (let* ((stream (binary-stream instance))
         (w-pointer (binary-w-pointer instance))
         (r-pointer (binary-r-pointer instance))
         (buffer-size (array-total-size buffer))
         (n (if (< (+ r-pointer buffer-size) w-pointer) buffer-size (- w-pointer r-pointer))))
    (declare (type fast-io:octet-vector stream) (type fixnum w-pointer r-pointer buffer-size n))

    (loop for i fixnum from r-pointer
          for j fixnum from 0 to (1- n)
          do (setf (aref buffer j) (aref stream i)))
    (incf (binary-r-pointer instance) n)
    n))

(defun binary-stream-read-sequence (instance buffer &optional start end)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary instance) (type fast-io:octet-vector buffer) (ignore start end))
  (binary-stream-read-into instance buffer))

(defun file-stream-to-binary-stream (stream)
  (declare (inline sb-sys:read-n-bytes) (type sb-sys:fd-stream stream))
  (let* ((size (file-length stream))
         (buffer (fast-io:make-octet-vector size)))
    (sb-sys:read-n-bytes stream buffer 0 size)
    (make-binary-stream :initial-data buffer :initial-size size :reuse-p t)))

