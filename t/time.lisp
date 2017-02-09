(in-package #:cl-user)
(defpackage #:clerk.time.test
  (:use #:cl #:prove))
(in-package #:clerk.time.test)

(plan 1)

(subtest "package clerk.time.test"
  (is (multiple-value-list
       (clerk.time::split-interval '1.minute))
      '(1 minute)
      "Can split numbered interval"
      :test #'equalp)

  (is (clerk.time::interval-type->seconds 'minute)
      60
      "Can convert interval-type to seconds")

  (is (clerk.time::interval->seconds '2.minutes)
      120
      "Can convert numbered interval to seconds")
  (is (clerk.time::interval->seconds '1.hour)
      360
      "Can convert numbered interval to seconds")
  (is (clerk.time::interval->seconds (list 1 'minute))
      60
      "Can convert a numbered interval presented as a list to seconds")

  (ok (typep (clerk.time::interval->seconds 'friday) 'number)
      "Days of the week return the remaining seconds until this time")

  (is (clerk.time:timejump 0 '1.minute)
      60
      "Can move the time with a numbered interval"))

(finalize)
