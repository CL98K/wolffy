(in-package #:pickle)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(defop +lsp-nil+ (env)
  (framer-write (gethash :framer env) +none+))

(defop +lsp-bool+ (env nil obj)
  (if (>= (the fixnum (gethash :proto env)) 2)
      (framer-write (gethash :framer env) (if obj +newtrue+ +newfalse+))
      (framer-write (gethash :framer env) (if obj +ture+ +false+))))

(defop +lsp-int+ (env nil obj)
  (let ((xobj obj))
    (declare (type fixnum xobj))
    (when (gethash :bin env)
      (if (>= xobj 0)
          (cond ((<= xobj #xff) (framer-write (gethash :framer env) +binint1+ obj) (return))
                ((<= xobj #xffff) (framer-write (gethash :framer env) +binint2+ (pack:pack "<H" obj)) (return))))

      (when (and (>= xobj #x-80000000) (<= xobj #x7fffffff))
        (framer-write (gethash :framer env) +binint+ (pack:pack "<i" obj))
        (return)))

    (when (>= (the fixnum (gethash :proto env)) 2)
      (let* ((encoded (encode-long obj))
             (n (array-total-size encoded)))
        (if (< n 256)
            (framer-write (gethash :framer env) +long1+ n encoded)
            (framer-write (gethash :framer env) +long4+ (pack:pack "<i" n) encoded))
        (return)))
    
    (if (and (>= xobj #x-80000000) (<= xobj #x7fffffff))
        (framer-write (gethash :framer env) +int+ (sb-ext:string-to-octets (write-to-string obj)) +newline+)
        (framer-write (gethash :framer env) +long+ (sb-ext:string-to-octets (write-to-string obj)) (char-code #\L) +newline+))))

(defop +lsp-float+ (env nil obj)
  (if (gethash :bin env)
      (framer-write (gethash :framer env) +binfloat+ (pack:pack ">d" obj))
      (framer-write (gethash :framer env) +float+ (sb-ext:string-to-octets (write-to-string obj)) +newline+)))

(defop +lsp-str+ (env nil obj)
  (if (gethash :bin env)
      (progn
        (let* ((encoded (sb-ext:string-to-octets obj :external-format :utf-8))
               (n (array-total-size encoded)))
          (declare (type simple-array encoded) (type fixnum n))
          (cond
            ((and (<= n #xff) (>= (the fixnum (gethash :proto env)) 4))
             (framer-write (gethash :framer env) +short-binunicode+ (pack:pack "<B" n) encoded))
            ((and (> n #xffffffff) (>= (the fixnum (gethash :proto env)) 4))
             (write-large-bytes (gethash :framer env) (list +binunicode8+ (pack:pack "<Q" n)) encoded))
            ((>= n (the fixnum (slot-value (gethash :framer env) 'frame-size-target)))
             (write-large-bytes (gethash :framer env) (list +binunicode+ (pack:pack "<I" n)) encoded))
            (t
             (framer-write (gethash :framer env) +binunicode+ (pack:pack "<I" n) encoded)))))
      (progn
        (let* ((objx (uiop/utility:frob-substrings obj "\\" "\\u005c"))
               (objx (uiop/utility:frob-substrings objx "\0" "\\u0000"))
               (objx (uiop/utility:frob-substrings objx "\n" "\\u000a"))
               (objx (uiop/utility:frob-substrings objx "\r" "\\u000d"))
               (objx (uiop/utility:frob-substrings objx "\xla" "\\u00la")))
          (framer-write (gethash :framer env) +unicode+ (sb-ext:string-to-octets objx :external-format :utf-8) +newline+)
          (setf obj objx))))
  (_memoize env obj))

(defop +lsp-list+ (env nil obj)
  (if (gethash :bin env) (framer-write (gethash :framer env) +empty-list+) (framer-write (gethash :framer env) +mark+ +list+))
  (_memoize env obj)
  (_batch_appends env obj))

(defop +lsp-hash-table+ (env nil obj)
  (if (gethash :bin env) (framer-write (gethash :framer env) +empty-dict+) (framer-write (gethash :framer env) +mark+ +dict+))
  (_memoize env obj)
  (_batch_setitems env obj))


@form
(defun _batch_appends (env items)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env) (type list items))

  (if (not (gethash :bin env))
      (progn
        (dolist (x items)
          (_save env x)
          (framer-write (gethash :framer env) +append+))
        (return-from _batch_appends)))

  (let* ((len (list-length items))
         (indexs (loop for i fixnum from 0 below len by (gethash :batchsize env) collect i)))

    (if (/= (the fixnum (first (last indexs))) len) (setf indexs (append indexs (list len))))
    
    (loop for first fixnum in indexs
          for i fixnum from 0 below (1- (list-length indexs))
          for second fixnum = (elt indexs (1+ i))
          for diff fixnum = (- second first)
          do
          (cond ((> diff 1)
                 (framer-write (gethash :framer env) +mark+)
                 (loop for i fixnum from first below second do (_save env (nth i items)))
                 (framer-write (gethash :framer env) +appends+))
                ((= diff 1)
                 (_save env (nth i items))
                 (framer-write (gethash :framer env) +append+)))

          (if (< diff (the fixnum (gethash :batchsize env))) (return-from _batch_appends)))))

@form
(defun _batch_setitems (env items)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env items))

  (let ((nitems (loop for key being the hash-keys of items using (hash-value value) collect (list key value))))
    
    (if (not (gethash :bin env))
        (progn
          (loop for (k v) in nitems
                do
                (_save env k)
                (_save env v)
                (framer-write (gethash :framer env) +setitem+))
          (return-from _batch_setitems)))

    (let* ((len (list-length nitems))
           (indexs (loop for i fixnum from 0 below len by (gethash :batchsize env) collect i)))
      
      (if (/= (the fixnum (first (last indexs))) len) (setf indexs (append indexs (list len))))
      
      (loop for first fixnum in indexs
            for i fixnum from 0 below (1- (list-length indexs))
            for second fixnum = (elt indexs (1+ i))
            for diff fixnum = (- second first)
            do
            (cond ((> diff 1)
                   (framer-write (gethash :framer env) +mark+)
                   (loop for i fixnum from first below second
                         for (k v) = (nth i nitems)
                         do
                         (_save env k)
                         (_save env v))
                   (framer-write (gethash :framer env) +setitems+))
                  ((= diff 1)
                   (destructuring-bind (k v) (nth i nitems)
                     (_save env k)
                     (_save env v))
                   (framer-write (gethash :framer env) +setitem+)))

            (if (< diff (the fixnum (gethash :batchsize env))) (return-from _batch_setitems))))))

@form
(defun _memoize (env obj)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env))
  
  (if (/= (the fixnum (gethash :fast env)) 0) (return-from _memoize))
  
  (let ((id (sb-kernel:get-lisp-obj-address obj))
        (idx (hash-table-count (gethash :memo env))))
    (declare (type fixnum id idx))
    (assert (not (gethash id (gethash :memo env))))
    (framer-write (gethash :framer env) (_put env idx))
    (setf (gethash id (gethash :memo env)) (list idx obj))))

@form
(defun _put (env obj)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env) (type t obj))
  
  (cond ((>= (the fixnum (gethash :proto env)) 4) +memoize+)
        ((gethash :bin env) (if (< (the fixnum obj) 256)
                   (list +binput+ (pack:pack "<B" obj))
                   (list +long-binput+ (pack:pack "<I" obj))))
        (t (list +put+ (sb-ext:string-to-octets obj) +newline+))))

@form
(defun _get (env obj)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env) (type t obj))
  
  (if (gethash :bin env)
      (if (< (the fixnum obj) 256)
          (list +binget+ (pack:pack "<B" obj))
          (list +long-binget+ (pack:pack "<I" obj)))
      (list +get+ (sb-ext:string-to-octets obj) +newline+)))

@form
(defun _save (env obj &key (save-persistent-id t))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type hash-table env) (type t obj) (type boolean save-persistent-id))

  (let* ((pid (_persistent_id obj)))

    (framer-commit (gethash :framer env))
    (when (and pid save-persistent-id)
      (_save_pers pid))

    (let ((x (gethash (the fixnum (sb-kernel:get-lisp-obj-address obj)) (gethash :memo env))))
      (when x
        (framer-write (gethash :framer env) (_get env (aref (the simple-vector x) 0)))
        (return-from _save)))

    (let ((op-code (type-to-code obj)))
      (declare (type fixnum op-code))
      (perform-op op-code env (slot-value (gethash :framer env) 'stream) obj))))

@form
(defun _persistent_id (obj)
  (declare (ignore obj)))

@form
(defun _save_pers (pid)
  (declare (ignore pid)))

(defun dump (obj file &key (protocol 0) (fix-imports t) (fast nil))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type t obj) (type string file) (type fixnum protocol) (type boolean fix-imports fast))
  
  (with-open-file (stream file :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede :if-does-not-exist :create)
    (multiple-value-bind (seq size) (dumps obj :protocol protocol :fix-imports fix-imports :fast fast)
      (write-sequence seq stream)
      size)))

(defun dumps (obj &key (protocol 0) (fix-imports t) (fast nil))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type t obj) (type fixnum protocol) (type boolean fix-imports fast))

  (when fast (return-from dumps (dump-fast-op obj protocol fix-imports)))

  (let* ((env (make-hash-table :test 'eq))
         (protocol (if (= protocol 0) *default-protocol* (if (< protocol 0) *highest-protocol* protocol)))
         (stream (wo-io:make-binary-stream))
         (framer (make-instance 'framer :stream stream :current-frame nil)))
    (declare (type hash-table env) (type fixnum protocol) (type wo-io:binary-stream stream) (type framer framer))

    (if (> protocol *highest-protocol*) (error 'value-error :message (format nil "pickle protocol must be <= ~A" *highest-protocol*)))
    
    (setf (gethash :proto env) protocol)
    (setf (gethash :bin env) (if (>= protocol 1) t nil))
    (setf (gethash :fast env) 0)
    (setf (gethash :fix-imports env) (if (and fix-imports (< protocol 3)) t nil))
    (setf (gethash :memo env) (make-hash-table :test 'eq))
    (setf (gethash :framer env) framer)
    (setf (gethash :batchsize env) 1000)
    
    (if (>= protocol 2)
        (framer-write framer +proto+ protocol))
    (if (>= protocol 4)
        (framer-start framer))
    
    (_save env obj)
    (framer-write framer +stop+)
    (framer-end framer)
    (wo-io:binary-stream-memery-view stream :show t)))

(define-fast-op dump-fast-op (obj protocol fix-imports) '(((gethash :fast env) . fast)
                                                          ((gethash :batchsize env) . batchsize)
                                                          ((gethash :proto env) . proto)
                                                          ((gethash :bin env) . bin)
                                                          ((gethash :fix-imports env) . fix-imports)
                                                          ((gethash :memo env) . memo)
                                                          ((gethash :framer env) . framer)
                                                          ((perform-op op-code env (slot-value (gethash :framer env) 'stream) obj) . *cond-exp*))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)) (type fixnum protocol) (type boolean fix-imports))
  
  (let* ((fast 0)
         (batchsize 1000)
         (protocol (if (= protocol 0) *default-protocol* (if (< protocol 0) *highest-protocol* protocol)))
         (proto protocol)
         (bin (if (>= protocol 1) t nil))
         (fix-imports (if (and fix-imports (< protocol 3)) t nil))
         (memo (make-hash-table :test 'equal))
         (stream (wo-io:make-binary-stream))
         (framer (make-instance 'framer :stream stream :current-frame nil)))
    (declare (type fixnum fast batchsize proto) (type (mod 255) protocol) (type boolean bin) (type hash-table memo) (type wo-io:binary-stream stream) (type framer framer) (ignore fix-imports))
    
    (if (> protocol *highest-protocol*) (error 'value-error :message (format nil "pickle protocol must be <= ~A" *highest-protocol*)))
    (if (>= protocol 2)
        (framer-write framer +proto+ protocol))
    (if (>= protocol 4)
        (framer-start framer))

    (labels *built-exp*
      (_save (make-hash-table) obj)
      (framer-write framer +stop+)
      (framer-end framer)
      (wo-io:binary-stream-memery-view stream :show t))))
