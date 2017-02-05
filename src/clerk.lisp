(in-package #:cl-user)
(defpackage #:clerk
  (:use #:cl)
  (:export #:*events*
           #:empty-events-queue
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

(defmethod initialize-instance :after ((event event) &key)
  (let ((fire-time (clerk.time:timejump (get-universal-time)
                                        (slot-value event 'interval))))
    
    (setf (slot-value event 'fire-time)
          fire-time)))

(defun continuous-p (type)
  "Only interval declared with `every` are considered continuous"
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
    (push event *events*)
    ;; sort events by fire-time
    (sort *events* #'< :key #'(lambda (e)
                                (slot-value e 'fire-time)))))
     
(defun empty-events-queue ()
  (setf *events* nil))
