# Clerk

A cron-like scheduler with sane DSL

## Example events

```
(event "Say 'Hi' all the time" every 5.seconds (print "Hi"))

(event "Compose and send monthly report"
       every 1.month (send-report (compose-monthly-report)))
```

## Instalation and usage

Clone the repo inside `quicklisp/local-projects` and do `(ql:quicklisp :clerk)` in your REPL.

Populate the `events.lisp` file with your strategies and do `(clerk:start)`. To monitor the event queue, simply type `(clerk:event-queue)`.

## Issues / Contribution

If you have issues - open a github issue or contact me at `(reverse "moc.liamg@vokist")`. If you want to contribute - open an issue or make a PR. Thanks!
