# Clerk

A cron-like scheduler with sane DSL

## Example events

```
(event "Say 'Hi' all the time" every 5.seconds (print "Hi"))

(event "Compose and send monthly report"
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

(event "Print farbe" every 3.seconds (write-to-file "Farbe" "log.txt"))
(event "Print colour" every 2.seconds (write-to-file "Colour" "log.txt"))
(event "Print @@@@ 1 min @@@@@" every 1.minute
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
/some time later.../
@@@@@@ 1 min @@@@@@
/etc.../
```

## Instalation and usage

Clone the repo inside `quicklisp/local-projects` and do `(ql:quicklisp :clerk)` in your REPL.

Make sure your events are loaded before executing `(clerk:start)`. The events reside inside `clerk:*events*`, but you can also type `(clerk:calendar)` to see a list of all scheduled and running events. 

## Issues / Contribution

If you have issues - open a github issue or contact me at `(reverse "moc.liamg@vokist")`. If you want to contribute - open an issue or make a PR. Thanks!
