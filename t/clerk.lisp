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

  (clerk:empty-events-queue)
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
    (clerk:empty-events-queue))
  
  (subtest "function (fire-event-p"
    (ok (not (clerk::fire-event-p
              (make-instance 'clerk:event
                             :interval '1.minute)))
        "Event is not fired before it's time")
    (ok (clerk::fire-event-p
         (make-instance 'clerk:event
                        :interval '-1.second))
        "Event is fired when the time comes"))

  (clerk:empty-events-queue)
    (clerk::empty-fired-events-list)
    (subtest "defmethod (fire-event"
      (clerk::fire-event
       (clerk:event "One-time event" in 1.second (+ 1 2)))
      (is (bt:join-thread (car clerk::*fired-events*))
          3
          "The event's calculation is performed successfully")
      (is (length clerk::*fired-events*) 1
          "The fired events queue has the fired event in it")
      (is (length clerk:*events*) 1
          "One-time events don't create a new event in the event queue
when they are fired.")
      (clerk:empty-events-queue)
      (clerk::empty-fired-events-list)
      (clerk::fire-event
       (clerk:event "Continuous event" every 1.second (+ 1 2)))
      (is (length clerk:*events*) 2
          "Continuous events create a new event in the event queue when
when they are fired")))

(finalize)
