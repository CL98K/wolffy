(in-package #:pickle)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(defop +lsp-nil+ (env)
  (framer-write (gethash :framer env) +none+))

(defop +lsp-bool+ (env nil obj)
  (let ((framer (gethash :framer env))
        (proto (gethash :proto env)))
    (declare (type fixnum proto))
    
    (if (>= proto 2)
        (framer-write framer (if obj +newtrue+ +newfalse+))
        (framer-write framer (if obj +ture+ +false+)))))

(defop +lsp-int+ (env nil obj)
  (let ((framer (gethash :framer env))
        (proto (gethash :proto env))
        (bin (gethash :bin env)))
    (declare (type fixnum obj proto bin))

    (when bin
      (if (>= obj 0)
          (cond ((<= obj #xff) (framer-write framer +binint1+ (pack:pack "<B" obj)) (return))
                ((<= obj #xffff) (framer-write framer +binint2+ (pack:pack "<H" obj)) (return))))

      (when (and (>= obj #x-80000000) (<= obj #x7fffffff))
        (framer-write framer +binint+ (pack:pack "<i" obj))
        (return)))

    (when (>= proto 2)
      (let* ((encoded (encode-long obj))
             (n (array-total-size encoded)))
        (if (< n 256)
            (framer-write framer +long1+ (pack:pack "<B" n) encoded)
            (framer-write framer +long4+ (pack:pack "<i" n) encoded))
        (return)))

    (if (and (>= obj #x-80000000) (<= obj #x7fffffff))
        (framer-write framer +int+ (sb-ext:string-to-octets (write-to-string obj)) +newline+)
        (framer-write framer +long+ (sb-ext:string-to-octets (write-to-string obj)) (char-code #\L) +newline+))))

(defop +lsp-float+ (env nil obj)
  (let ((framer (gethash :framer env))
        (bin (gethash :bin env)))
    (if bin
        (framer-write framer +binfloat+ (pack:pack ">d" obj))
        (framer-write framer +float+ (sb-ext:string-to-octets (write-to-string obj)) +newline+))))

(defop +lsp-str+ (env nil obj)
  (let ((framer (gethash :framer env))
        (proto (gethash :proto env))
        (bin (gethash :bin env)))
    (declare (type fixnum proto) (type boolean bin))
    (if bin
        (progn
          (let* ((encoded (sb-ext:string-to-octets obj :external-format :utf-8))
                 (n (array-total-size encoded)))
            (declare (type simple-array encoded) (type fixnum n))
            (cond
              ((and (<= n #xff) (>= proto 4))
               (framer-write framer +short-binunicode+ (pack:pack "<B" n) encoded))
              ((and (> n #xffffffff) (>= proto 4))
               (write-large-bytes framer (list +binunicode8+ (pack:pack "<Q" n)) encoded))
              ((>= n (the fixnum (slot-value framer 'frame-size-target)))
               (write-large-bytes framer (list +binunicode+ (pack:pack "<I" n)) encoded))
              (t
               (framer-write framer +binunicode+ (pack:pack "<I" n) encoded)))))
        (progn
          (let* ((objx (uiop/utility:frob-substrings obj "\\" "\\u005c"))
                 (objx (uiop/utility:frob-substrings objx "\0" "\\u0000"))
                 (objx (uiop/utility:frob-substrings objx "\n" "\\u000a"))
                 (objx (uiop/utility:frob-substrings objx "\r" "\\u000d"))
                 (objx (uiop/utility:frob-substrings objx "\xla" "\\u00la")))
            (framer-write framer +unicode+ (sb-ext:string-to-octets objx :external-format :utf-8) +newline+)
            (setf obj objx))))
    (_memoize env obj)))

(defop +lsp-list+ (env nil obj)
  (let ((framer (gethash :framer env))
        (bin (gethash :bin env)))
    (if bin (framer-write framer +empty-list+) (framer-write framer +mark+ +list+))
    (_memoize env obj)
    (_batch_appends env obj)))

(defop +lsp-hash-table+ (env nil obj)
  (let ((framer (gethash :framer env))
        (bin (gethash :bin env)))
    (if bin (framer-write framer +empty-dict+) (framer-write framer +mark+ +dict+))
    (_memoize env obj)
    (_batch_setitems env obj)))


(declaim (ftype (function (hash-table t) t) _memoize) (inline _memoize))
(defun _memoize (env obj)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env) (type t obj))
  
  (if (/= (the fixnum (gethash :fast env)) 0) (return-from _memoize))
  
  (let ((id (sb-kernel:get-lisp-obj-address obj))
        (idx (hash-table-count (gethash :memo env))))
    (declare (type fixnum id idx))
    (assert (not (gethash id (gethash :memo env))))
    (framer-write (gethash :framer env) (_put env idx))
    (setf (gethash id (gethash :memo env)) (list idx obj))))

(declaim (ftype (function (hash-table t) t) _put) (inline _put))
(defun _put (env obj)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env) (type t obj))
  
  (cond ((>= (the fixnum (gethash :proto env)) 4) +memoize+)
        ((gethash :bin env) (if (< (the fixnum obj) 256)
                   (list +binput+ (pack:pack "<B" obj))
                   (list +long-binput+ (pack:pack "<I" obj))))
        (t (print obj) (list +put+ (sb-ext:string-to-octets obj) +newline+))))

(declaim (ftype (function (hash-table t) t) _get) (inline _get))
(defun _get (env obj)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env) (type t obj))
  
  (if (gethash :bin env)
      (if (< (the fixnum obj) 256)
          (list +binget+ (pack:pack "<B" obj))
          (list +long-binget+ (pack:pack "<I" obj)))
      (list +get+ (sb-ext:string-to-octets obj) +newline+)))

(declaim (ftype (function (hash-table t &key (:save-persistent-id boolean)) t) _save) (inline _save))
(defun _save (env obj &key (save-persistent-id t))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env) (type t obj) (type boolean save-persistent-id))
  
  (let* ((framer (gethash :framer env))
         (stream (slot-value framer 'stream))
         (pid (_persistent_id obj)))

    (framer-commit framer)
    (when (and pid save-persistent-id)
      (_save_pers pid))

    (let ((x (gethash (the fixnum (sb-kernel:get-lisp-obj-address obj)) (gethash :memo env))))
      (when x
        (framer-write framer (_get env (aref (the simple-vector x) 0)))
        (return-from _save)))

    (perform-op (type-to-code obj) env stream obj)))

(defun _persistent_id (obj) nil)

(defun _save_pers (pid) nil)

(declaim (ftype (function (hash-table list) t) _batch_appends) (inline _batch_appends))
(defun _batch_appends (env items)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env) (type list items))
  
  (let ((framer (gethash :framer env))
        (bin (gethash :bin env))
        (batchsize (gethash :batchsize env)))
    
    (if (not bin)
        (progn
          (dolist (x items)
            (_save env x)
            (framer-write framer +append+))
          (return-from _batch_appends)))

    (let* ((len (list-length items))
           (indexs (loop for i fixnum from 0 below len by batchsize collect i)))

      (if (/= (the fixnum (first (last indexs))) len) (setf indexs (append indexs (list len))))
      
      (loop for first fixnum in indexs
            for i fixnum from 0 below (1- (list-length indexs))
            for second fixnum = (elt indexs (1+ i))
            for diff fixnum = (- second first)
            do
            (cond ((> diff 1)
                   (framer-write framer +mark+)
                   (loop for i fixnum from first below second do (_save env (nth i items)))
                   (framer-write framer +appends+))
                  ((= diff 1)
                   (_save env (nth i items))
                   (framer-write framer +append+)))

            (if (< diff batchsize) (return-from _batch_appends))))))

(declaim (ftype (function (hash-table hash-table) t) _batch_setitems) (inline _batch_setitems))
(defun _batch_setitems (env items)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env items))

  (let ((framer (gethash :framer env))
        (bin (gethash :bin env))
        (batchsize (gethash :batchsize env))
        (nitems (loop for key being the hash-keys of items using (hash-value value) collect (list key value))))
    
    (if (not bin)
        (progn
          (loop for (k v) in nitems
                do
                (_save env k)
                (_save env v)
                (framer-write framer +setitem+))
          (return-from _batch_setitems)))

    (let* ((len (list-length nitems))
           (indexs (loop for i fixnum from 0 below len by batchsize collect i)))
      
      (if (/= (the fixnum (first (last indexs))) len) (setf indexs (append indexs (list len))))
      
      (loop for first fixnum in indexs
            for i fixnum from 0 below (1- (list-length indexs))
            for second fixnum = (elt indexs (1+ i))
            for diff fixnum = (- second first)
            do
            (cond ((> diff 1)
                   (framer-write framer +mark+)
                   (loop for i fixnum from first below second
                         for (k v) = (nth i nitems)
                         do
                         (_save env k)
                         (_save env v))
                   (framer-write framer +setitems+))
                  ((= diff 1)
                   (destructuring-bind (k v) (nth i nitems)
                     (_save env k)
                     (_save env v))
                   (framer-write framer +setitem+)))

            (if (< diff batchsize) (return-from _batch_setitems))))))

(defun dump (obj file &key (protocol 0) (fix-imports t))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type t obj) (type string file) (type fixnum protocol) (type boolean fix-imports))
  
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (dumps obj :stream stream :protocol protocol :fix-imports fix-imports)))

(defun dumps (obj &key (stream (wo-io:make-binary-stream)) (protocol 0) (fix-imports t))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type t obj) (type wo-io:binary-stream stream) (type fixnum protocol) (type boolean fix-imports))

  (let ((env (make-hash-table :test 'eq))
        (protocol (if (= protocol 0) *default-protocol* (if (< protocol 0) *highest-protocol* protocol)))
        (framer (make-instance 'framer :stream stream :current-frame nil)))
    (declare (type hash-table env) (type fixnum protocol))

    (if (> protocol *highest-protocol*) (error 'value-error :message (format nil "pickle protocol must be <= ~A" *highest-protocol*)))
    
    (setf (gethash :proto env) protocol)
    (setf (gethash :bin env) (if (>= protocol 1) t nil))
    (setf (gethash :fast env) 0)
    (setf (gethash :fix-imports env) (if (and fix-imports (< protocol 3)) t nil))
    (setf (gethash :memo env) (make-hash-table :test 'eq))
    (setf (gethash :framer env) framer)
    (setf (gethash :batchsize env) 1000)
    
    (if (>= protocol 2)
        (framer-write framer +proto+ (pack:pack "<B" protocol)))
    (if (>= protocol 4)
        (framer-start framer))

    (_save env obj)
    (framer-write framer +stop+)
    (framer-end framer)
    (wo-io:binary-stream-memery-view stream)))

;; (define-fast-op dump-fast-op (stream protocol fix-imports)
;;   (let* ((fast 0)
;;          (batchsize 1000)        
;;          (protocol (if (= protocol 0) *default-protocol* (if (< protocol 0) *highest-protocol* protocol)))
;;          (proto protocol)
;;          (bin (if (>= protocol 1) t nil))
;;          (fix-imports (if (and fix-imports (< protocol 3)) t nil))
;;          (memo (make-hash-table :test 'equal))
;;          (framer (make-instance 'framer :stream stream :current-frame nil)))

;;     (declare (type fixnum fast batchsize protocol proto) (type boolean bin fix-imports) (type hash-table memo) (type framer framer))
    
;;     (if (> protocol *highest-protocol*) (error 'value-error :message (format nil "pickle protocol must be <= ~A" *highest-protocol*)))

;;     (if (>= protocol 2)
;;         (framer-write framer +proto+ (pack:pack "<B" protocol)))
;;     (if (>= protocol 4)
;;         (framer-start framer))

;;     (_save env obj)
;;     (framer-write framer +stop+)
;;     (framer-end framer)))

