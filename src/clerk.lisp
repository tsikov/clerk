(in-package #:cl-user)
(defpackage #:clerk
  (:use #:cl)
  (:export #:*events*
           #:event))
(in-package #:clerk)

;; TODO:
;; display calendar

(defparameter *events* nil)

(defclass event ()
  ((name :initarg :name)
   (interval :initarg :interval)
   (fire-time :initarg :fire-time)
   (body :initarg :body)))

(defclass continuous-event (event) ())
(defclass one-time-event (event) ())

(defun continuous-p (type)
  ;; string= will do package agnostic symbol comparison
  (string= type 'every))

(defun make-event (name type interval body)
  (let ((event-class (if (continuous-p type)
                         'continuous-event
                         'one-time-event)))
    (make-instance event-class
                   :name name
                   :interval interval
                   :body body)))

(defmacro event (name type interval body)
  `(add-to-event-queue ,name ',type ',interval ',body))

(defun add-to-event-queue (name type interval body)
  (let ((event (make-event name type interval body)))
    (push event *events*)))
