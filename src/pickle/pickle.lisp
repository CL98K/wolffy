(in-package #:wo-pickle)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(defparameter *format-version* "4.0" "File format version we write")
(defparameter *compatible-formats '(1.0 1.1 1.2 1.3 2.0 3.0 4.0 5.0) "")
(defparameter *highest-protocol* 5 "This is the highest protocol number we know how to read.")
(defparameter *default-protocol* 4 "The protocol we write by default.")

(declaim (inline +sys-maxsize+))
(defconstant +sys-maxsize+ 9223372036854775807)

(declaim (inline encode-long))
(defun encode-long (x)
  (if (= x 0) (return-from encode-long (char-code #\ )))
  (let* ((nbytes (1+ (ash (integer-length x) -3)))
         (result (integer-to-bytes x nbytes :order :little :signed t))
         (length (array-total-size result)))
    
    (if (and (< x 0) (> nbytes 1))
        (if (and (= (aref result (- length 1)) 255) (/= (aref result (- length 2)) 0))
            (return-from encode-long (make-array (1- length) :displaced-to result :displaced-index-offset 0))))

    (return-from encode-long result)))

(declaim (inline decode-long))
(defun decode-long (array &key (order :little) (signed t))
  (bytes-to-integer array :order order :signed signed))


(defclass framer ()
  ((stream :initarg :stream)
   (current-frame :initarg :current-frame)
   (frame-size-min :initform 4 :reader frame-size-min :allocation :class)
   (frame-size-target :initform (* 64 1024) :reader frame-size-target :allocation :class)))

(defmethod start-framing ((instance framer))
  (setf (slot-value instance 'current-frame) (wo-io:make-binary-stream)))

(defmethod end-framing ((instance framer))
  (let ((current-frame (slot-value instance 'current-frame)))
    (if (and current-frame (wo-io:binary-stream-file-position current-frame))
        (progn
          (commit-frame instance :force t)
          (setf (slot-value instance 'current-frame) nil)))))

(defmethod commit-frame ((instance framer) &key force)
  (with-slots (stream current-frame frame-size-min frame-size-target) instance
    (when current-frame
      (when (or (>= (wo-io:binary-stream-file-position current-frame) frame-size-target) force)
        (multiple-value-bind (data size) (wo-io:binary-stream-memery-view current-frame)
          (if (>= size frame-size-min)
              (progn
                (wo-io:binary-stream-write stream +frame+)
                (wo-io:binary-stream-writes stream (pack:pack "<Q" size))))
          (wo-io:binary-stream-writes stream data)
          (setf current-frame (wo-io:make-binary-stream)))))))

(defmethod write-frame ((instance framer) &rest datas)
  (let* ((current-frame (slot-value instance 'current-frame))
         (stream (if current-frame current-frame (slot-value instance 'stream))))

    (loop for data in datas
          do
          (if (typep data '(mod 255))
              (wo-io:binary-stream-writes stream data)
              (wo-io:binary-stream-writes stream data)))))

(defmethod write-large-bytes ((instance framer) header payload)
  (let ((current-frame (slot-value instance 'current-frame)))
    (if current-frame (commit-frame instance :force t))
    (wo-io:binary-stream-writes current-frame header)
    (wo-io:binary-stream-writes current-frame payload)))


(defclass unframer ()
  ((current-frame :initarg :current-frame :accessor current-frame-of)))

(defmethod framer-read ((instance unframer) stream n)
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type stream wo-io:binary-stream) (type fixnum n))

  (let ((current-frame (slot-value instance 'current-frame)))
    (if current-frame
        (progn
          (let ((data (wo-io:binary-stream-reads current-frame n)))
            (declare (type simple-vector data))
            (when (and (= (array-total-size data) 0) (/= n 0))
              (setf (slot-value instance 'current-frame) nil)
              (let ((buffer (make-array n :element-type '(unsigned-byte 8))))
                (wo-io:binary-stream-read-sequence stream buffer)
                (return-from framer-read buffer)))
            (when (< (array-total-size data) n)
              (error 'unpickling-error :message "pickle exhausted before end of frame"))
            (return-from framer-read data)))
        (progn
          (let ((buffer (make-array n :element-type '(unsigned-byte 8))))
            (wo-io:binary-stream-read-sequence stream buffer)
            (return-from framer-read buffer))))))

(defmethod framer-read-into ((instance unframer) stream buffer)
  (let ((current-frame (slot-value instance 'current-frame)))
    (if current-frame
        (progn
          (let ((n (wo-io:binary-stream-read-into current-frame buffer))
                (buf-size (array-total-size buffer)))
            (when (and (= n 0) (/= buf-size 0))
              (setf (slot-value instance 'current-frame) nil)
              (wo-io:binary-stream-read-sequence stream buffer)
              (return-from framer-read-into buf-size))
            (when (< n buf-size)
              (error 'unpickling-error :message "pickle exhausted before end of frame"))
            (return-from framer-read-into n)))
        (progn
          (wo-io:binary-stream-read-sequence stream buffer)
          (return-from framer-read-into (array-total-size buffer))))))

(defmethod framer-read-line ((instance unframer) stream)
  (let ((current-frame (slot-value instance 'current-frame)))
    (if current-frame
        (progn
          (let ((data (wo-io:binary-stream-read-line current-frame)))
            (when (= (array-total-size data) 0)
              (setf (slot-value instance 'current-frame) nil)
              (return-from framer-read-line (read-line stream)))
            (when (/= (svref data (1- (array-total-size data))) (char-code #\NEWLINE))
              (error 'unpickling-error :message "pickle exhausted before end of frame"))
            (return-from framer-read-line data)))
        (progn          
          (return-from framer-read-line (read-line stream))))))

(defmethod framer-load-frame ((instance unframer) stream frame-size)
  (let ((current-frame (slot-value instance 'current-frame))
        (buffer (make-array frame-size :element-type '(unsigned-byte 8))))
    (if (and current-frame (wo-io:binary-stream-read current-frame))
        (error 'unpickling-error :message "beginning of a new frame before end of current frame"))

    (wo-io:binary-stream-read-sequence stream buffer)
    (setf (slot-value instance 'current-frame) (wo-io:make-binary-stream :initial-data buffer))))
