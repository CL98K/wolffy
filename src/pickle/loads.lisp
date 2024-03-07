(in-package #:pickle)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(defop +proto+ (env stream)
  (let ((proto (aref (the (simple-array (unsigned-byte 8) *) (framer-read (gethash :framer env) stream 1)) 0)))
    (declare (type fixnum proto))
    (if (not (and (>= proto 0) (<= proto (the fixnum *highest-protocol*))))
        (error 'value-error :message (format nil "unsupported pickle protocol: ~A" proto))
        (setf (gethash :proto env) proto))))

(defop +frame+ (env stream)
  (let ((frame-size (first (pack:unpack "<Q" (framer-read (gethash :framer env) stream 8)))))
    (declare (type fixnum frame-size))
    (if (> frame-size +sys-maxsize+)
        (error 'value-error :message (format nil "frame size > sys.maxsize: ~A" frame-size))
        (framer-load-frame (gethash :framer env) stream frame-size))))

;; (defop +persid+ (env stream))

(defop +binpersid+ (env)
  (push (persistent-load (pop (gethash :stack env))) (gethash :stack env)))

(defop +none+ (env)
  (push :none (gethash :stack env)))

(defop +newfalse+ (env)
  (push :false (gethash :stack env)))

(defop +newtrue+ (env)
  (push :ture (gethash :stack env)))

;; 待补充
;; (defop +int+ (env stream))

(defop +binint+ (env stream)
  (push (first (pack:unpack "<i" (framer-read (gethash :framer env) stream 4))) (gethash :stack env)))

(defop +binint1+ (env stream)
  (push (aref (the (simple-array (unsigned-byte 8) *) (framer-read (gethash :framer env) stream 1)) 0) (gethash :stack env)))

(defop +binint2+ (env stream)
  (push (first (pack:unpack "<H" (framer-read (gethash :framer env) stream 2))) (gethash :stack env)))

;; 待补充
;; (defop +long+ (env stream))

(defop +long1+ (env stream)
  (let* ((framer (gethash :framer env))
         (n (aref (the (simple-array (unsigned-byte 8) *) (framer-read framer stream 1)) 0)))
    (push (decode-long (the (simple-array (unsigned-byte 8) *) (framer-read framer stream n))) (gethash :stack env))))

(defop +long4+ (env stream)
  (let* ((framer (gethash :framer env))
         (n (first (pack:unpack "<i" (framer-read framer stream 4)))))
    (declare (type fixnum n))
    (if (< n 0) (error 'unpickling-error :message "LONG pickle has negative byte count"))
    (push (decode-long (the (simple-array (unsigned-byte 8) *) (framer-read framer stream n))) (gethash :stack env))))

;; 待补充
;; (defop +float+ (env stream))

(defop +binfloat+ (env stream)
  (push (first (pack:unpack ">d" (framer-read (gethash :framer env) stream 8))) (gethash :stack env)))

;; 待补充
;; (defop +string+ (env stream))

(defop +binstring+ (env stream)
  (let* ((framer (gethash :framer env))
         (n (first (pack:unpack "<i" (framer-read framer stream 4)))))
    (declare (type fixnum n))
    (if (< n 0) (error 'unpickling-error :message "BINSTRING pickle has negative byte count"))
    (push (decode-string (framer-read framer stream n)) (gethash :stack env))))

(defop +binbytes+ (env stream)
  (let* ((framer (gethash :framer env))
         (n (first (pack:unpack "<I" (framer-read framer stream 4)))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'unpickling-error :message "BINBYTES exceeds system's maximum size"))
    (push (framer-read framer stream n) (gethash :stack env))))

;; 待补充
;; (defop +unicode+ (env stream))

(defop +binunicode+ (env stream)
  (let* ((framer (gethash :framer env))
         (n (first (pack:unpack "<I" (framer-read framer stream 4)))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'unpickling-error :message "BINUNICODE exceeds system's maximum size"))
    (push (sb-ext:octets-to-string (framer-read framer stream n)) (gethash :stack env))))

(defop +binunicode8+ (env stream)
  (let* ((framer (gethash :framer env))
         (n (first (pack:unpack "<Q" (framer-read framer stream 8)))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'unpickling-error :message "BINUNICODE8 exceeds system's maximum size"))
    (push (sb-ext:octets-to-string (framer-read framer stream n)) (gethash :stack env))))

(defop +binbytes8+ (env stream)
  (let* ((framer (gethash :framer env))
         (n (first (pack:unpack "<Q" (framer-read framer stream 8)))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'unpickling-error :message "BINBYTES8 exceeds system's maximum size"))
    (push (framer-read framer stream n) (gethash :stack env))))

(defop +bytearray8+ (env stream)
  (let* ((framer (gethash :framer env))
         (n (first (pack:unpack "<Q" (framer-read framer stream 8))))
         (buffer (make-array n :element-type '(unsigned-byte 8))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'unpickling-error :message "BYTEARRAY8 exceeds system's maximum size"))
    (push (framer-read-into framer stream buffer) (gethash :stack env))))

;; (defop +next-buffer+ (env stream))
;; (defop +readonly-buffer+ (env stream))

(defop +short-binstring+ (env stream)
  (let ((framer (gethash :framer env)))
    (push (decode-string (framer-read framer stream (aref (the (simple-array (unsigned-byte 8) *) (framer-read framer stream 1)) 0))) (gethash :stack env))))

(defop +short-binbytes+ (env stream)
  (let ((framer (gethash :framer env)))
    (push (framer-read framer stream (aref (the (simple-array (unsigned-byte 8) *) (framer-read framer stream 1)) 0)) (gethash :stack env))))

(defop +short-binunicode+ (env stream)
  (let ((framer (gethash :framer env)))
    (push (sb-ext:octets-to-string (framer-read framer stream (aref (the (simple-array (unsigned-byte 8) *) (framer-read framer stream 1)) 0))) (gethash :stack env))))

(defop +tuple+ (env)
  (push (pop-mark env) (gethash :stack env)))

(defop +empty-tuple+ (env)
  (push '() (gethash :stack env)))

(defop +tuple1+ (env)
  (push (list (pop (gethash :stack env))) (gethash :stack env)))

(defop +tuple2+ (env)
  (let ((first-val (pop (gethash :stack env)))
        (secode-val (pop (gethash :stack env))))
    (push (list secode-val first-val) (gethash :stack env))))

(defop +tuple3+ (env)
  (let ((first-val (pop (gethash :stack env)))
        (secode-val (pop (gethash :stack env)))
        (three-val (pop (gethash :stack env))))
    (push (list three-val secode-val first-val) (gethash :stack env))))

(defop +empty-list+ (env)
  (push '() (gethash :stack env)))

(defop +empty-dict+ (env)
  (push (make-hash-table :test 'equal) (gethash :stack env)))

(defop +empty-set+ (env)
  (push '() (gethash :stack env)))

(defop +frozenset+ (env)
  (push (pop-mark env) (gethash :stack env)))

(defop +list+ (env)
  (push (pop-mark env) (gethash :stack env)))

(defop +dict+ (env)
  (let ((items (pop-mark env))
        (hash-table (make-hash-table :test 'equal)))
    (loop for (k v) on items by #'cddr do (setf (gethash k hash-table) v))
    (push hash-table (gethash :stack env))))

;; (defop +inst+ (env stream))
;; (defop +obj+ (env stream))
;; (defop +newobj+ (env stream))
;; (defop +newobj-ex+ (env stream))
;; (defop +global+ (env stream))
;; (defop +stack-global+ (env stream))
;; (defop +ext1+ (env stream))
;; (defop +ext2+ (env stream))
;; (defop +ext4+ (env stream))

(defop +reduce+ (env)
  (let ((args (pop (gethash :stack env)))
        (func (first (gethash :stack env))))
    (setf (first (gethash :stack env)) (funcall func args))))

(defop +pop+ (env)
  (let ((stack (gethash :stack env)))
    (if stack (setf (gethash :stack env) (cdr stack)) (pop-mark env))))

(defop +pop-mark+ (env)
  (pop-mark env))

(defop +dup+ (env)
  (push (first (gethash :stack env)) (gethash :stack env)))

;; 待补充
;; (defop +get+ (env stream))

(defop +binget+ (env stream)
  (push (gethash (aref (the (simple-array (unsigned-byte 8) *) (framer-read (gethash :framer env) stream 1)) 0) (gethash :memo env)) (gethash :stack env)))

(defop +long-binget+ (env stream)
  (push (gethash (first (pack:unpack "<I" (framer-read (gethash :framer env) stream 4))) (gethash :memo env)) (gethash :stack env)))

;; 待补充
;; (defop +put+ (env stream))

(defop +binput+ (env stream)
  (let ((i (aref (the (simple-array (unsigned-byte 8) *) (framer-read (gethash :framer env) stream 1)) 0)))
    (declare (type fixnum i))
    (if (< i 0) (error 'value-error :message "negative BINPUT argument"))
    (setf (gethash i (gethash :memo env)) (first (gethash :stack env)))))

(defop +long-binput+ (env stream)
  (let ((n (first (pack:unpack "<I" (framer-read (gethash :framer env) stream 4)))))
    (declare (type fixnum n))
    (if (> n +sys-maxsize+) (error 'value-error :message "negative LONG_BINPUT argument"))
    (setf (gethash n (gethash :memo env)) (first (gethash :stack env)))))

(defop +memoize+ (env)
  (setf (gethash (hash-table-count (gethash :memo env)) (gethash :memo env)) (first (gethash :stack env))))

(defop +append+ (env)
  (push (pop (gethash :stack env)) (first (gethash :stack env))))

(defop +appends+ (env)
  (let ((items (pop-mark env)))
    (alexandria:appendf (first (gethash :stack env)) items)))

(defop +setitem+ (env)
  (let ((val (pop (gethash :stack env)))
        (key (pop (gethash :stack env))))
    (setf (gethash key (first (gethash :stack env))) val)))

(defop +setitems+ (env)
  (let ((items (pop-mark env))
        (hash-table (first (gethash :stack env))))
    (loop for (k v) on items by #'cddr do (setf (gethash k hash-table) v))))

(defop +additems+ (env)
  (let ((items (pop-mark env)))
    (setf (first (gethash :stack env)) (pairlis items items (first (gethash :stack env))))))

;; (defop +build+ (env stream))

(defop +mark+ (env)
  (push (gethash :stack env) (gethash :meta-stack env))
  (setf (gethash :stack env) '()))

(defop +stop+ (env)
  (signal 'stop :value (pop (gethash :stack env))))


(declaim (ftype (function (t) boolean) decode-string) (inline decode-string))
(defun decode-string (value)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)) (ignore value))
  t)

(declaim (ftype (function (hash-table) sequence) pop-mark) (inline pop-mark))
(defun pop-mark (env)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

  (let ((items (gethash :stack env)))
    (setf (gethash :stack env) (pop (gethash :meta-stack env)))
    (nreverse items)))

(declaim (ftype (function (t) error) persistent-load) (inline persistent-load))
(defun persistent-load (pid)
  (error 'unpickling-error :message "unsupported persistent id encountered"))

(defun load (file &key (fix-imports t) (element-type "ascii") (fast nil))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))
  
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (loads (wo-io:file-stream-to-binary-stream stream) :fix-imports fix-imports :element-type element-type :fast fast)))

(defun loads (stream &key (fix-imports t) (element-type "ascii") (fast nil))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)) (type boolean fast) (ignore fix-imports element-type))

  (when fast (return-from loads (load-fast-op stream)))
  
  (let ((env (make-hash-table :test 'eq)))
    (setf (gethash :proto env) 0)
    (setf (gethash :stack env) nil)
    (setf (gethash :meta-stack env) nil)
    (setf (gethash :memo env) (make-hash-table :test 'equal))
    (setf (gethash :framer env) (make-instance 'unframer :current-frame nil))
    
    (handler-case
        (loop for op-code = (framer-read (gethash :framer env) stream 1)
              while op-code
              do
              (perform-op (aref (the (simple-array (unsigned-byte 8) *) op-code) 0) env stream nil))
      (stop (condition)
        (return-from loads (stop-value condition))))
    
    (error 'unpickling-error :message "Reached end of file before reading +STOP+ op code")))

(define-fast-op load-fast-op (stream)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))
  
  (let ((proto 0)
        (stack nil)
        (meta-stack nil)
        (memo (make-hash-table :test 'equal))
        (framer (make-instance 'unframer :current-frame nil)))
    (declare (type fixnum proto) (type list stack meta-stack) (type hash-table memo) (type unframer framer))
    
    (handler-case
        (loop for op-code fixnum = (aref (the (simple-array (unsigned-byte 8) *) (framer-read framer stream 1)) 0)
              while op-code
              do *cond-exp*)
      (stop (condition)
        (return-from load-fast-op (stop-value condition))))
    (error 'unpickling-error :message "Reached end of file before reading +STOP+ op code")))
