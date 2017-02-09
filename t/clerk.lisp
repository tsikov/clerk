(in-package #:cl-user)
(defpackage #:clerk.test
  (:use #:cl #:prove))
(in-package #:clerk.test)

(plan 1)

(subtest "package clerk.test"
  (subtest "function (make-job"
    (is (type-of (clerk::make-job "Friendly job"
                                    'every
                                    '5.minutes
                                    '(print "Hi!")))
        'continuous-job
        "Can make continuous job"
        :test #'string=)
    (is (type-of (clerk::make-job "Friendly job"
                                    'in
                                    '1.day
                                    '(print "Hi!")))
        'one-time-job
        "Can make one-time job"
        :test #'string=))

  (clerk:empty-jobs-queue)
  (subtest "macro (job ..."
    (clerk:job "Cool job" every 5.days (print "Party!"))
    (is (length clerk:*jobs*)
        1
        "Adds an job to the jobs queue.")
    
    (clerk:empty-jobs-queue)
    (clerk:job "First job to fire"
               in 1.minute (print "Fire!"))
    (clerk:job "Second job to fire"
               in 2.minutes (print "Fire!"))
    (with-slots (clerk::name) (first clerk:*jobs*)
      (is clerk::name "First job to fire"
          "Orders jobs by time of firing."
          :test #'string=)))

  (subtest "function (job-fn"
    (clerk:empty-jobs-queue)
    (clerk:job-fn "Test job-fn" 'every '1.minute #'(lambda () (print "Fire!")))
    (with-slots (clerk::name) (first clerk:*jobs*)
      (is clerk::name "Test job-fn"
          "Adds the job to the job queue."
          :test #'string=))
    (clerk:job-fn "Test job-fn (interval as a list)"
                  'in
                  (list 5 'seconds)
                  #'(lambda () (print "Fire!")))
    (with-slots (clerk::name) (first clerk:*jobs*)
      (is clerk::name "Test job-fn (interval as a list)"
          "Adds the job to the job queue. Can decipher interval as a list"
          :test #'string=)))
  
  (clerk:empty-jobs-queue)
  (subtest "function (fire-job-p"
    (ok (not (clerk::fire-job-p
              (make-instance 'clerk:job
                             :interval '1.minute)))
        "Job is not fired before it's time")
    (ok (clerk::fire-job-p
         (make-instance 'clerk:job
                        :interval '-1.second))
        "Job is fired when the time comes"))

  (clerk:empty-jobs-queue)
  (subtest "defmethod (fire-job"
    (let ((job-thread (clerk::fire-job
                         (clerk:job "One-time job" in 1.second (+ 1 2)))))
      (is (bt:join-thread job-thread)
          3
          "The job's calculation is performed successfully"))
    (is (length clerk:*jobs*) 1
        "One-time jobs don't create a new job in the job queue
when they are fired.")
    (clerk:empty-jobs-queue)
    (clerk::fire-job
     (clerk:job "Continuous job" every 1.second (+ 1 2)))
    (is (length clerk:*jobs*) 2
        "Continuous jobs create a new job in the job queue when
when they are fired")))

(finalize)
