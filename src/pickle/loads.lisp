(in-package #:wo-pickle)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(defparameter *proto* 0)
(defparameter *meta-stack* '())
(defparameter *stack* '())
(defparameter *memo* (make-hash-table :test 'equal))
(defparameter *framer* nil)

(defun init-load-env ()
  (setf *proto* 0)
  (setf *meta-stack* '())
  (setf *stack* '())
  (clrhash *memo*)
  (setf *framer* (make-instance 'unframer :current-frame nil)))

(defun py-load (file &key (fix-imports t) (element-type "ascii"))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))
  
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (py-loads (wo-io:file-stream-to-binary-stream stream) :fix-imports fix-imports :element-type element-type)))

(defun py-loads (stream &key (fix-imports t) (element-type "ascii"))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)) (ignore fix-imports element-type))
  
  (init-load-env)
  (handler-case
      (loop for op-code = (framer-read *framer* stream 1)
            while op-code
            do
            (perform-op (aref op-code 0) stream nil))
    (stop (condition)
      (clrhash *memo*)
      (return-from py-loads (stop-value condition))))
  (error 'unpickling-error :message "Reached end of file before reading +STOP+ op code"))

(defun pop-mark ()
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))
  
  (let ((items *stack*))
    (setf *stack* (pop *meta-stack*))
    (nreverse items)))

(defun persistent-load (pid)
  (error 'unpickling-error :message "unsupported persistent id encountered"))

(defun decode-string (value) t)

(defop +proto+ (stream)
  (let ((proto (aref (framer-read *framer* stream 1) 0)))
    (if (not (and (>= proto 0) (<= proto *highest-protocol*)))
        (error 'value-error :message (format nil "unsupported pickle protocol: ~A" proto))
        (setf *proto* proto))))

(defop +frame+ (stream)
  (let ((frame-size (first (pack:unpack "<Q" (framer-read *framer* stream 8)))))
    (declare (type fixnum frame-size))
    (if (> frame-size +sys-maxsize+)
        (error 'value-error :message (format nil "frame size > sys.maxsize: ~A" frame-size))
        (framer-load-frame *framer* stream frame-size))))

;; (defop +persid+ (stream))

(defop +binpersid+ ()
  (push (persistent-load (pop *stack*)) *stack*))

(defop +none+ ()
  (push :none *stack*))

(defop +newfalse+ ()
  (push :false *stack*))

(defop +newtrue+ ()
  (push :ture *stack*))

;; (defop +int+ (stream))

(defop +binint+ (stream)
  (push (first (pack:unpack "<i" (framer-read *framer* stream 4))) *stack*))

(defop +binint1+ (stream)
  (push (aref (framer-read *framer* stream 1) 0) *stack*))

(defop +binint2+ (stream)
  (push (first (pack:unpack "<H" (framer-read *framer* stream 2))) *stack*))

;; (defop +long+ (stream))

(defop +long1+ (stream)
  (let ((n (aref (framer-read *framer* stream 1) 0)))
    (push (decode-long (coerce (framer-read *framer* stream n) 'simple-vector)) *stack*)))

(defop +long4+ (stream)
  (let ((n (first (pack:unpack "<i" (framer-read *framer* stream 4)))))
    (declare (type fixnum n))
    (if (< n 0) (error 'unpickling-error :message "LONG pickle has negative byte count"))
    (push (decode-long (framer-read *framer* stream n)) *stack*)))

;; (defop +float+ (stream))

(defop +binfloat+ (stream)
  (push (first (pack:unpack ">d" (framer-read *framer* stream 8))) *stack*))

;; (defop +string+ (stream))

(defop +binstring+ (stream)
  (let ((n (first (pack:unpack "<i" (framer-read *framer* stream 4)))))
    (declare (type fixnum n))
    (if (< n 0) (error 'unpickling-error :message "BINSTRING pickle has negative byte count"))
    (push (decode-string (framer-read *framer* stream n)) *stack*)))

(defop +binbytes+ (stream)
  (let ((n (first (pack:unpack "<I" (framer-read *framer* stream 4)))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'unpickling-error :message "BINBYTES exceeds system's maximum size"))
    (push (framer-read *framer* stream n) *stack*)))

;; (defop +unicode+ (stream))

(defop +binunicode+ (stream)
  (let ((n (first (pack:unpack "<I" (framer-read *framer* stream 4)))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'unpickling-error :message "BINUNICODE exceeds system's maximum size"))
    (push (sb-ext:octets-to-string (framer-read *framer* stream n)) *stack*)))

(defop +binunicode8+ (stream)
  (let ((n (first (pack:unpack "<Q" (framer-read *framer* stream 8)))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'unpickling-error :message "BINUNICODE8 exceeds system's maximum size"))
    (push (sb-ext:octets-to-string (framer-read *framer* stream n)) *stack*)))

(defop +binbytes8+ (stream)
  (let ((n (first (pack:unpack "<Q" (framer-read *framer* stream 8)))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'unpickling-error :message "BINBYTES8 exceeds system's maximum size"))
    (push (framer-read *framer* stream n) *stack*)))

(defop +bytearray8+ (stream)
  (let* ((n (first (pack:unpack "<Q" (framer-read *framer* stream 8))))
         (buffer (make-array n :element-type '(unsigned-byte 8))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'unpickling-error :message "BYTEARRAY8 exceeds system's maximum size"))
    (push (framer-read-into *framer* stream buffer) *stack*)))

;; (defop +next-buffer+ (stream))
;; (defop +readonly-buffer+ (stream))

(defop +short-binstring+ (stream)
  (push (decode-string (framer-read *framer* stream (aref (framer-read *framer* stream 1) 0))) *stack*))

(defop +short-binbytes+ (stream)
  (push (framer-read *framer* stream (aref (framer-read *framer* stream 1) 0)) *stack*))

(defop +short-binunicode+ (stream)
  (push (sb-ext:octets-to-string (framer-read *framer* stream (aref (framer-read *framer* stream 1) 0))) *stack*))

(defop +tuple+ ()
  (push (pop-mark) *stack*))

(defop +empty-tuple+ ()
  (push '() *stack*))

(defop +tuple1+ ()
  (push (list (pop *stack*)) *stack*))

(defop +tuple2+ ()
  (let ((first-val (pop *stack*))
        (secode-val (pop *stack*)))
    (push (list secode-val first-val) *stack*)))

(defop +tuple3+ ()
  (let ((first-val (pop *stack*))
        (secode-val (pop *stack*))
        (three-val (pop *stack*)))
    (push (list three-val secode-val first-val) *stack*)))

(defop +empty-list+ ()
  (push '() *stack*))

(defop +empty-dict+ ()
  (push (make-hash-table :test 'equal) *stack*))

(defop +empty-set+ ()
  (push '() *stack*))

;; (defop +frozenset+ (stream))

(defop +list+ ()
  (push (pop-mark) *stack*))

(defop +dict+ (stream)
  (let ((items (pop-mark))
        (hash-table (make-hash-table :test 'equal)))
    (loop for i from 0 to (length items) by 2
          do (setf (gethash (nth i items) hash-table) (nth (1+ i) items)))
    (push hash-table *stack*)))

;; (defop +inst+ (stream))
;; (defop +obj+ (stream))
;; (defop +newobj+ (stream))
;; (defop +newobj-ex+ (stream))
;; (defop +global+ (stream))
;; (defop +stack-global+ (stream))
;; (defop +ext1+ (stream))
;; (defop +ext2+ (stream))
;; (defop +ext4+ (stream))

(defop +reduce+ (stream)
  (let ((args (pop *stack*))
        (func (first *stack*)))
    (setf (first *stack*) (func args))))

(defop +pop+ ()
  (if *stack* (setf *stack* (cdr *stack*)) (pop-mark)))

(defop +pop-mark+ ()
  (pop-mark))

(defop +dup+ ()
  (push (first *stack*) *stack*))

;; (defop +get+ (stream))

(defop +binget+ (stream)  
  (push (gethash (aref (framer-read *framer* stream 1) 0) *memo*) *stack*))

(defop +long-binget+ (stream)
  (push (gethash (first (pack:unpack "<I" (framer-read *framer* stream 4))) *memo*) *stack*))

;; (defop +put+ (stream))

(defop +binput+ (stream)
  (let ((i (aref (framer-read *framer* stream 1) 0)))
    (declare (type fixnum i))
    (if (< i 0) (error 'value-error :message "negative BINPUT argument"))
    (setf (gethash i *memo*) (first *stack*))))

(defop +long-binput+ (stream)
  (let ((n (first (pack:unpack "<I" (framer-read *framer* stream 4)))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'value-error :message "negative LONG_BINPUT argument"))
    (setf (gethash n *memo*) (first *stack*))))

(defop +memoize+ ()
  (setf (gethash (hash-table-count *memo*) *memo*) (first *stack*)))

(defop +append+ ()
  (push (pop *stack*) (first *stack*)))

(defop +appends+ ()
  (let ((items (pop-mark)))
    (alexandria:appendf (first *stack*) items)))

(defop +setitem+ ()
  (let ((val (pop *stack*))
        (key (pop *stack*)))
    (setf (gethash key (first *stack*)) val)))

(defop +setitems+ ()
  (let ((items (pop-mark)))
    (loop for i fixnum from 0 to (1- (length items)) by 2
          do (setf (gethash (nth i items) (first *stack*)) (nth (1+ i) items)))))

(defop +additems+ ()
  (let ((items (pop-mark)))
    (setf (first *stack*) (pairlis items items (first *stack*)))))

;; (defop +build+ (stream))

(defop +mark+ ()
  (push *stack* *meta-stack*)
  (setf *stack* '()))

(defop +stop+ ()
  (signal 'stop :value (pop *stack*)))

