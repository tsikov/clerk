# Clerk

A cron-like scheduler with sane DSL

## Example usage

```
(job "Say 'Hi' all the time" every 5.seconds (print "Hi"))

(job "Compose and send monthly report"
     every 1.month (send-report (compose-monthly-report)))
```

If you want to see it with your eyes, make sure to load the following code:

```
(defun write-to-file (msg file)
  (with-open-file (log file
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format log "~A~%" msg)))

(job "Print farbe" every 3.seconds (write-to-file "Farbe" "log.txt"))
(job "Print colour" every 2.seconds (write-to-file "Colour" "log.txt"))
(job "Print @@@@ 1 min @@@@@" every 1.minute
       (write-to-file "@@@@@@ 1 min @@@@@@" "log.txt"))
```
Now, after `(clerk:start)`, tailing `log.txt` should give you something like this:

```
Colour
Farbe
Colour
Colour
Farbe
Colour
Farbe
Colour
Colour
Farbe
Colour
/one minute later.../
@@@@@@ 1 min @@@@@@
/etc.../
```

## Instalation and usage

Clone the repo inside `quicklisp/local-projects` and do `(ql:quicklisp :clerk)` in your REPL.

Make sure your jobs are loaded before executing `(clerk:start)`. The jobs reside inside `clerk:*jobs*`, but you can also type `(clerk:calendar)` to see a list of all scheduled and running jobs. 

## Job types

There are two types of jobs - `continuous` and `one-time`. If a job has the keyword `every` after the job description - the job will be countinuous. This means that when an event is fired, a new event will be pushed in the event queue for firing exactly `interval` time from now. The jobs above are an example for `continuous` jobs.

A `one-time` job is fired once and then it is removed from the jobs' queue. An example for a one-time job can be:

```
(job "Extraordinary event" in 5.days (send-mail "Don't forget X"))
```

You can use any word instead of `in`.

## Intervals

Right now (more are coming soon) there is one type of interval in the form of:

```
[number].[interval-type]
```

where the number is a positive integer and the `interval-type` if one of the following: `second`, `minute`, `hour`, `day`, `week`, `month`, `year`. Also you can use the plural form of all these words. For example `1.second` and `2.seconds` are both valid.

## Issues / Contribution

If you have issues - open a github issue or contact me at `(reverse "moc.liamg@vokist")`. If you want to contribute - open an issue or make a PR. Thanks!
