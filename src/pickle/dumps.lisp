(in-package #:wo-pickle)

(defparameter *proto* 0)
(defparameter *bin* nil)
(defparameter *fast* 0)
(defparameter *fix-imports* nil)
(defparameter *memo* (make-hash-table :test 'eq))
(defparameter *framer* nil)
(defparameter *_batchsize* 1000)

(defun lisp-dump (obj file &key protocol (fix-imports t))
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (lisp-dumps obj :stream stream :protocol protocol :fix-imports fix-imports)))

(defun lisp-dumps (obj &key (stream (wo-io:make-binary-stream)) protocol (fix-imports t))
  ;;init env  
  (let ((protocol (if (null protocol) *default-protocol* (if (< protocol 0) *highest-protocol* protocol))))
    (setf *proto* protocol)
    (setf *bin* (if (>= protocol 1) t nil))
    (setf *fast* 0)
    (setf *fix-imports* (if (and fix-imports (< protocol 3)) t nil))
    (clrhash *memo*)
    (setf *framer* (make-instance 'framer :stream stream :current-frame nil)))
  
  (if (>= *proto* 2)
      (write-frame *framer* +proto+ (pack:pack "<B" *proto*)))
  (if (>= *proto* 4)
      (start-framing *framer*))

  (_save obj)
  (write-frame *framer* +stop+)
  (end-framing *framer*))

(defun _memoize (obj)
  (if (/= *fast* 0) (return-from _memoize))
  
  (let ((id (sb-kernel:get-lisp-obj-address obj))
        (idx (hash-table-count *memo*)))    
    (assert (not (gethash id *memo*)))
    (write-frame *framer* (_put idx))
    (setf (gethash id *memo*) (list idx obj))))

(defun _put (idx)
  (cond ((>= *proto* 4) +memoize+)
        (*bin* (if (< idx 256)
                   (list +binput+ (pack:pack "<B" idx))
                   (list +long-binput+ (pack:pack "<I" idx))))
        (t (list +put+ (map 'vector #'char-code idx) (char-code #\NEWLINE)))))

(defun _get (i)
  (if *bin*
      (if (< i 256)
          (list +binget+ (pack:pack "<B" i))
          (list +long-binget+ (pack:pack "<I" i)))
      (list +get+ (map 'vector #'char-code idx) (char-code #\NEWLINE))))

;; (declaim (ftype (function (t &key (:save-persistent-id boole)) t) _save) (inline _save))
(defun _save (obj &key (save-persistent-id t))
  (commit-frame *framer*)

  (let ((x (gethash (sb-kernel:get-lisp-obj-address obj) *memo*)))
    (when x
      (write-frame *framer* (_get (aref x 0)))
      (return-from _save)))
  
  (perform-op (type-map obj) nil obj))

(defun _batch_appends (items)
  (if (not *bin*)
      (progn
        (dolist (x items)
          (_save x)
          (write-frame *framer* +append+))
        (return-from _batch_appends)))

  (let* ((len (length items))
         (indexs (loop for i from 0 below len by *_batchsize* collect i)))

    (if (/= (car (last indexs)) len) (setf indexs (append indexs (list len))))
    
    (loop for first in indexs
          for i from 0 below (1- (length indexs))
          for second = (elt indexs (1+ i))
          for diff = (- second first)
          do
          (cond ((> diff 1)
                 (write-frame *framer* +mark+)
                 (loop for i from first below second do (_save (elt items i)))
                 (write-frame *framer* +appends+))
                ((= diff 1)
                 (_save (elt items i))
                 (write-frame *framer* +append+)))

          (if (< diff *_batchsize*) (return-from _batch_appends)))))

;; (defun _batch_setitems (items)
;;   (if (not *bin*)
;;       (progn
;;         (loop for (k v) in items
;;               do
;;               (_save k)
;;               (_save v)
;;               (write-frame *framer* +setitem+))
;;         (return-from _batch_appends)))


;;   )


;;;; implementation
(defop +lsp-nil+ (stream obj)
  (write-frame *framer* +none+))

(defop +lsp-bool+ (stream obj)
  (if (>= *proto* 2)
      (write-frame *framer* (if obj +newtrue+ +newfalse+))
      (write-frame *framer* (if obj +ture+ +false+))))

(defop +lsp-int+ (stream obj)
  (if *bin*
      (progn
        (if (>= obj 0)
            (cond ((<= obj #xff) (write-frame *framer* +binint1+ (pack:pack "<B" obj)) (return))
                  ((<= obj #xffff) (write-frame *framer* +binint2+ (pack:pack "<H" obj)) (return))))
        
        (if (and (>= obj #x-80000000) (<= obj #x7fffffff))
            (progn
              (write-frame *framer* +binint+ (pack:pack "<i" obj))
              (return)))))

  (if (>= *proto* 2)
      (let* ((encoded (encode-long obj))
             (n (array-total-size encoded)))
        
        (if (< n 256)
            (write-frame *framer* +long1+ (pack:pack "<B" n) encoded)
            (write-frame *framer* +long4+ (pack:pack "<i" n) encoded))
        (return)))

  (if (and (>= obj #x-80000000) (<= obj #x7fffffff))
      (write-frame *framer* +int+ (map 'vector #'char-code (write-to-string obj)) (char-code #\NEWLINE))
      (write-frame *framer* +long+ (map 'vector #'char-code (write-to-string obj)) (char-code #\L) (char-code #\NEWLINE))))

(defop +lsp-float+ (stream obj)
  (if *bin*
      (write-frame *framer* +binfloat+ (pack:pack ">d" obj))
      (write-frame *framer* +float+ (map 'vector #'char-code (write-to-string obj)) (char-code #\NEWLINE))))

(defop +lsp-str+ (stream obj)
  (if *bin*
      (progn
        (let* ((encoded (map 'vector #'char-code obj))
               (n (array-total-size encoded)))
          (cond
            ((and (<= n #xff) (>= *proto* 4))
             (write-frame *framer* +short-binunicode+ (pack:pack "<B" n) encoded))
            ((and (> n #xffffffff) (>= *proto* 4))
             (write-large-bytes *framer* +binunicode8+ (pack:pack "<Q" n) encoded))
            ((>= n (slot-value *framer* 'frame-size-target))
             (write-large-bytes *framer* +binunicode+ (pack:pack "<I" n) encode))
            (t
             (write-frame *framer* +binunicode+ (pack:pack "<I" n) encoded)))))
      (progn
        (let* ((objx (uiop/utility:frob-substrings obj "\\" "\\u005c"))
               (objx (uiop/utility:frob-substrings obj "\0" "\\u0000"))
               (objx (uiop/utility:frob-substrings obj "\n" "\\u000a"))
               (objx (uiop/utility:frob-substrings obj "\r" "\\u000d"))
               (objx (uiop/utility:frob-substrings obj "\xla" "\\u00la")))
          (write-frame *framer* +unicode+ (map 'vector #'char-code "raw-unicode-escape") (char-code #\NEWLINE)))))
  (_memoize obj))

;; (defop +lsp-tuple+ (stream obj))

(defop +lsp-list+ (stream obj)
  (if *bin* (write-frame *framer* +empty-list+) (write-frame *framer* +mark+ +list+))
  (_memoize obj)
  (_batch_appends obj))

;; (defop +lsp-dict+ (stream obj)
;;   )
