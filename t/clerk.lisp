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
                                    'in
                                    '1.day
                                    '(print "Hi!")))
        'one-time-event
        "Can make one-time event"
        :test #'string=))

    (subtest "macro (event ..."
      (clerk:event "Cool event" every 5.days (print "Party!"))
      (is (length clerk:*events*)
          1
          "Adds an event to the events queue.")

      (clerk:empty-events-queue)
      (clerk:event "First event to fire"
                   in 1.minute (print "Fire!"))
      (clerk:event "Second event to fire"
                   in 2.minutes (print "Fire!"))
      (with-slots (clerk::name) (first clerk:*events*)
        (is clerk::name "First event to fire"
            "Orders events by time of firing."
            :test #'string=))
      ;; clear the event queue again for future tests
      (clerk:empty-events-queue)))

(finalize)
