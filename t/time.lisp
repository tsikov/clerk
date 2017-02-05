(in-package #:cl-user)
(defpackage #:clerk.time.test
  (:use #:cl #:prove))
(in-package #:clerk.time.test)

(plan 1)

(subtest "package clerk.time.test"

  (is (multiple-value-list
       (clerk.time::split-interval '1.minute))
      '(1 minute)
      :test #'equalp)

  (is (clerk.time::interval-type->seconds 'minute)
      60)

  (is (clerk.time::interval->seconds '2.minutes)
      120)
  (is (clerk.time::interval->seconds '1.hour)
      360)

  (is (clerk.time:timejump 0 '1.minute)
      60))

(finalize)
