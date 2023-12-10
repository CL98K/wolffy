(in-package #:wo-pickle)

(define-condition pickle-error (error)
  ((message :initarg :message
            :accessor pickle-error-message)))

(define-condition pickling-error (pickle-error) ()
  (:report (lambda (condition stream)
             (format stream "Pickle error: ~A" (pickle-error-message condition)))))

(define-condition unpickling-error (pickle-error) ()
  (:report (lambda (condition stream)
             (format stream "UNPickle error: ~A" (pickle-error-message condition)))))

(define-condition value-error (error)
  ((message :initarg :message
            :accessor value-error-message))
  (:report (lambda (condition stream)
             (format stream "Value error: ~A" (pickle-error-message condition)))))

(define-condition stop ()
  ((value :initarg :value
          :accessor stop-value)))
