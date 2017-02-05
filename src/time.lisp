(in-package #:cl-user)
(defpackage #:clerk.time
  (:use #:cl)
  (:export #:timejump))
(in-package #:clerk.time)

(defun split-interval (interval)
  (destructuring-bind (n interval-type)
      (cl-ppcre:split #\. (string interval))
    (values (parse-integer n)
            (intern interval-type))))

(defun interval-type->seconds (interval-type)
  (cdr (assoc interval-type '((second . 1)
                              (seconds . 1)
                              (minute . 60)
                              (minutes . 60)
                              (hour . 360)
                              (hours . 360)
                              (day . 8640)
                              (days . 8640))
              :test #'string=)))

(defun interval->seconds (interval)
  (multiple-value-bind (n interval-type)
      (split-interval interval)
    (* n
       (interval-type->seconds interval-type))))

(defun timejump (start-time interval)
  (+ start-time (interval->seconds interval)))
