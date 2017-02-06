(in-package #:cl-user)
(defpackage #:clerk
  (:use #:cl)
  (:export #:*events*
           #:empty-events-queue
           #:event
           #:start
           #:stop
           #:calendar))
(in-package #:clerk)

;; TODO:
;; rename event -> job

(defparameter *events* nil
  "All scheduled events")
(defparameter *main-thread* nil)

(defclass event ()
  ((name :initarg :name :reader name)
   (interval :initarg :interval)
   (fire-time :initarg :fire-time :reader fire-time)
   (body :initarg :body :reader body)))

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
  `(add-to-event-queue ,name ',type ',interval
                       (lambda () ,body)))

(defun add-to-event-queue (name type interval fn)
  (let ((event (make-event name type interval fn)))
    (push event *events*)
    (sort *events* #'< :key #'fire-time)
    event))

(defun empty-events-queue ()
  (setf *events* nil))

(defun fire-event-p (event)
  "Check if it is time to fire an event"
  (<= (fire-time event) (get-universal-time)))

(defmethod fire-event ((event event))
  (bt:make-thread (body event) :name (name event)))

(defmethod fire-event :before ((event continuous-event))
  "Create the next event in the event queue when firing continuous
events."
  (with-slots (name interval body) event
    (add-to-event-queue name 'every interval body)))

(defun fire-event-if-needed ()
  (if (fire-event-p (car *events*))
      (progn
        (fire-event (pop *events*))
        ;; just in case the second event in queue is the same
        ;; second as the first one. Or there might be a lot of
        ;; events in the queue.
        (fire-event-if-needed))))

(defun start ()
  "Start the thread that waits for an event to fire."
  (setf *main-thread*
        (bt:make-thread
         #'(lambda ()
             (loop
                (fire-event-if-needed)
                (sleep 1)))
         :name "Main scheduler thread.")))

(defun stop ()
  "Stop scheduler"
  (bt:destroy-thread *main-thread*)
  (setf *main-thread* nil))

(defun calendar (&optional (stream *standard-output*))
  "Print pending and fired events"
  (format stream "PENDING EVENTS:~%")
  (loop for event in *events*
     do (with-slots (name interval fire-time) event
            (format stream "~A - ~A - ~A~%" name interval fire-time))))

