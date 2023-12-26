(in-package #:io)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(declaim (inline +newline+))
(defconstant +newline+ (char-code #\newline))

(defstruct binary
  (stream nil :type fast-io:octet-vector)
  (size 0 :type (mod 67108864))
  (restream-size 1.5 :type single-float :read-only t)
  (w-pointer 0 :type (mod 67108864))
  (r-pointer 0 :type (mod 67108864))
  (s-pointer 0 :type (mod 67108864))
  (separator nil))

(defclass binary-stream (sb-gray:fundamental-binary-stream)
  ((binary :initarg :binary :type binary)))

(defun make-binary-stream (&key (initial-data (make-array 0)) (initial-size 128) (restream-size 1.5) (upgrade-p t))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type (or simple-vector fast-io:octet-vector) initial-data) (type fixnum initial-size) (type single-float restream-size) (type boolean upgrade-p))

  (let* ((instance nil)
         (size (array-total-size initial-data))
         (initial-size (if (> size 0) size initial-size))
         (stream (fast-io:make-octet-vector initial-size)))
    (setf instance (make-instance 'binary-stream
                                  :binary (make-binary :stream stream :size initial-size :restream-size restream-size)))
    (if (> size 0) (binary-stream-writes instance initial-data :upgrade-p upgrade-p))
    instance))

(defun binary-stream-close (instance)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance))

  (setf (slot-value instance 'binary) nil))

(defun binary-stream-info (instance)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance))

  (slot-value instance 'binary))

(defun binary-stream-memery-view (instance)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance))
  
  (let* ((binary (slot-value instance 'binary))
         (stream (binary-stream binary))
         (w-pointer (binary-w-pointer binary))
         (buffer (fast-io:make-octet-vector w-pointer)))
    (declare (type binary binary) (type fast-io:octet-vector stream buffer) (type (mod 67108864) w-pointer))

    (fast-io:fast-read-sequence buffer (fast-io:make-input-buffer :vector stream))
    (values buffer w-pointer)))

(defun binary-stream-file-position (instance &optional position-spec)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance) (type (or fixnum null) position-spec))

  (let* ((binary (slot-value instance 'binary))
         (sep (binary-separator binary)))
    (declare (type binary binary) (type list sep))
    
    (when position-spec
      (setf (binary-r-pointer binary) position-spec)
      (setf (binary-s-pointer binary) (loop for i fixnum downfrom (1- (length sep)) to 0
                                            for j fixnum = (nth i sep)
                                            for x fixnum from 0
                                            while (< j position-spec) finally (return x))))
    (binary-r-pointer binary)))

(defun binary-stream-upgrade-space (instance &optional (size 0))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance) (type fixnum size))

  (let* ((binary (slot-value instance 'binary))
         (stream (binary-stream binary))
         (stream-size (binary-size binary))
         (restream-size (the single-float (binary-restream-size binary)))
         (upgrade-size (floor (* (if (= size 0) stream-size size) restream-size)))
         (buffer (fast-io:make-octet-vector upgrade-size)))
    (declare (type binary binary) (type fast-io:octet-vector stream buffer) (type (mod 67108864) stream-size upgrade-size) (type float restream-size))

    (loop for i fixnum from 0 below stream-size do (setf (aref buffer i) (aref stream i)))

    (setf (binary-stream binary) buffer)
    (setf (binary-size binary) (1- upgrade-size))))

(defun binary-stream-write (instance integer)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance) (type (mod 256) integer))

  (let* ((binary (slot-value instance 'binary))
         (stream (binary-stream binary))
         (stream-size (binary-size binary))
         (w-pointer (binary-w-pointer binary))
         (sep (binary-separator binary)))
    (declare (type binary binary) (type fast-io:octet-vector stream) (type (mod 67108864) stream-size w-pointer) (type list sep))
    
    (if (>= w-pointer stream-size) (binary-stream-upgrade-space instance))
    (if (eq integer +newline+) (push w-pointer sep))

    (setf (aref stream w-pointer) integer)
    (incf (binary-w-pointer binary))))

(defun binary-stream-writes (instance integers &key (upgrade-p t))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance) (type (or simple-vector fast-io:octet-vector) integers) (type boolean upgrade-p))

  (let* ((binary (slot-value instance 'binary))
         (stream (binary-stream binary))
         (stream-size (binary-size binary))
         (w-pointer (binary-w-pointer binary))
         (data-size (array-total-size integers)))
    (declare (type binary binary) (type fast-io:octet-vector stream) (type (mod 67108864) data-size stream-size w-pointer))

    (when (and upgrade-p (<= (- (- stream-size w-pointer) data-size) 0))
      (binary-stream-upgrade-space instance (+ stream-size data-size))
      (setf stream (binary-stream binary)))
    
    (loop for integer fixnum across integers
          for i fixnum from w-pointer
          do
          (setf (aref stream i) integer)
          (if (eq integer +newline+) (push i (binary-separator binary))))
    
    (incf (binary-w-pointer binary) data-size)))

(defun binary-stream-read (instance)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance))
  
  (let* ((binary (slot-value instance 'binary))
         (stream (binary-stream binary))
         (w-pointer (binary-w-pointer binary))
         (r-pointer (binary-r-pointer binary)))
    (declare (type binary binary) (type fast-io:octet-vector stream) (type (mod 67108864) w-pointer r-pointer))
    
    (if (>= r-pointer w-pointer) (return-from binary-stream-read nil))
    (prog1 (aref stream r-pointer)
      (incf (binary-r-pointer binary)))))

(defun binary-stream-reads (instance &optional (n 1))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance) (type fixnum n))
  
  (let* ((binary (slot-value instance 'binary))
         (stream (binary-stream binary))
         (w-pointer (binary-w-pointer binary))
         (r-pointer (binary-r-pointer binary))
         (diff (- w-pointer r-pointer))
         (n (if (> diff n) n diff))
         (buffer (fast-io:make-octet-vector n)))
    (declare (type binary binary) (type fast-io:octet-vector stream buffer) (type (mod 67108864) w-pointer r-pointer diff n))
    
    (prog1 buffer
      (loop for i fixnum from r-pointer
            for j fixnum from 0 to (1- n)
            do (setf (aref buffer j) (aref stream i)))
      (incf (binary-r-pointer binary) n))))

(defun binary-stream-read-line (instance)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance))
  
  (let* ((binary (slot-value instance 'binary))
         (stream (binary-stream binary))
         (w-pointer (binary-w-pointer binary))
         (r-pointer (binary-r-pointer binary))
         (s-pointer (binary-s-pointer binary))
         (sep (binary-separator binary))
         (sep-size (length sep))
         (index (if sep (nth (- sep-size s-pointer 1) sep) w-pointer))
         (n (if (/= index w-pointer) (if (> index r-pointer) (1+ (- index r-pointer)) 0) (- w-pointer r-pointer)))
         (buffer (fast-io:make-octet-vector n)))
    (declare (type binary binary) (type fast-io:octet-vector stream buffer) (type list sep) (type (mod 67108864) w-pointer r-pointer sep-size index n))

    (when (and (= n 0) (> w-pointer r-pointer))
      (setf n (- w-pointer r-pointer))
      (setf buffer (fast-io:make-octet-vector n)))
    
    (prog1 buffer
      (loop for i fixnum from r-pointer
            for j fixnum from 0 to (1- n)
            do (setf (aref buffer j) (aref stream i)))
      (incf (binary-r-pointer binary) n)
      (if (< (1+ s-pointer) sep-size) (incf (binary-s-pointer binary))))))

(defun binary-stream-read-into (instance buffer)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance) (type (simple-array (unsigned-byte 8) *) buffer))
  
  (let* ((binary (slot-value instance 'binary))
         (stream (binary-stream binary))
         (w-pointer (binary-w-pointer binary))
         (r-pointer (binary-r-pointer binary))
         (buffer-size (array-total-size buffer))
         (n (if (< (+ r-pointer buffer-size) w-pointer) buffer-size (- w-pointer r-pointer))))
    (declare (type binary binary) (type fast-io:octet-vector stream) (type (mod 67108864) w-pointer r-pointer buffer-size n))

    (loop for i fixnum from r-pointer
          for j fixnum from 0 to (1- n)
          do (setf (aref buffer j) (aref stream i)))
    (incf (binary-r-pointer binary) n)
    n))

(defun binary-stream-read-sequence (instance buffer &optional start end)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type binary-stream instance) (type (simple-array (unsigned-byte 8) *) buffer) (ignore start end))
  (binary-stream-read-into instance buffer))

(defun file-stream-to-binary-stream (stream)
  (declare (type sb-sys:fd-stream stream))
  (let ((buffer (fast-io:make-octet-vector (file-length stream))))
    (fast-io:fast-read-sequence buffer (fast-io:make-input-buffer :stream stream))
    (make-binary-stream :initial-data buffer)))
