(in-package #:cl-user)
(defpackage #:clerk.test
  (:use #:cl #:prove))
(in-package #:clerk.test)

(plan 1)

(subtest "package clerk.test"
  (subtest "function (make-event"
    (is (type-of (clerk::make-event "Friendly event"
                                    'every
                                    '5.minutes
                                    '(print "Hi!")))
        'continuous-event
        "Can make continuous event"
        :test #'string=)
    (is (type-of (clerk::make-event "Friendly event"
                                    'on
                                    'monday
                                    '(print "Hi!")))
        'one-time-event
        "Can make one-time event"
        :test #'string=))

    (subtest "macro (event ..."
      (clerk:event "Cool event" every friday (print "Party!"))
      (is (length clerk:*events*)
          1
          "Adds an event to the events queue.")))
  
(finalize)
