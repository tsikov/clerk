(in-package #:cl-user)
(defpackage #:clerk
  (:use #:cl)
  (:export #:*jobs*
           #:empty-jobs-queue
           #:job
           #:job-fn
           #:start
           #:stop
           #:calendar))
(in-package #:clerk)

(defparameter *jobs* nil
  "All scheduled jobs")
(defparameter *main-thread* nil)

(defclass job ()
  ((name :initarg :name :reader name)
   (interval :initarg :interval :reader interval)
   (fire-time :initarg :fire-time :accessor fire-time)
   (body :initarg :body :reader body)))

(defclass continuous-job (job) ())
(defclass one-time-job (job) ())

(defmethod initialize-instance :after ((job job) &key)
  (let ((fire-time (clerk.time:timejump (get-universal-time)
                                        (interval job))))    
    (setf (fire-time job)
          fire-time)))

(defun continuous-p (type)
  "Only interval declared with `every` are considered continuous"
  ;; string= will do package agnostic symbol comparison
  (string= type 'every))

(defun make-job (name type interval body)
  (let ((job-class (if (continuous-p type)
                       'continuous-job
                       'one-time-job)))
    (make-instance job-class
                   :name name
                   :interval interval
                   :body body)))

(defmacro job (name type interval body)
  `(add-to-jobs-queue ,name ',type ',interval
                       (lambda () ,body)))

(defun job-fn (name type interval fn)
  (add-to-jobs-queue name type interval fn))

(defun add-to-jobs-queue (name type interval fn)
  (let ((job (make-job name type interval fn)))
    (push job *jobs*)
    (sort *jobs* #'< :key #'fire-time)
    job))

(defun empty-jobs-queue ()
  (setf *jobs* nil))

(defun fire-job-p (job)
  "Check if it is time to fire a job"
  (<= (fire-time job) (get-universal-time)))

(defmethod fire-job ((job job))
  (bt:make-thread (body job) :name (name job)))

(defmethod fire-job :before ((job continuous-job))
  "Create the next job in the job queue when firing continuous
jobs."
  (with-slots (name interval body) job
    (add-to-jobs-queue name 'every interval body)))

(defun fire-job-if-needed ()
  (if (and *jobs* (fire-job-p (car *jobs*)))
      (progn
        (fire-job (pop *jobs*))
        ;; just in case the second job in queue is the same
        ;; second as the first one. Or there might be a lot of
        ;; jobs in the queue.
        (fire-job-if-needed))))

(defun start ()
  "Start the thread that waits for a jobs to fire."
  (setf *main-thread*
        (bt:make-thread
         #'(lambda ()
             (loop
                (fire-job-if-needed)
                (sleep 1)))
         :name "Main scheduler thread.")))

(defun stop ()
  "Stop scheduler"
  (bt:destroy-thread *main-thread*)
  (setf *main-thread* nil))

(defun calendar (&optional (stream *standard-output*))
  "Print the scheduled jobs"
  (format stream "JOBS:~%")
  (loop for job in *jobs*
     do (with-slots (name interval fire-time) job
          (format stream "~A - ~A - ~A~%" name interval fire-time))))

