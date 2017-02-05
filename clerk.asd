(in-package #:cl-user)
(defpackage #:clerk-asd
  (:use #:cl #:asdf))
(in-package #:clerk-asd)

(defsystem #:clerk
  :version      "0.1.0"
  :description  "A cron-like scheduler with sane DSL"
  :author       "Petko Tsikov <tsikov@gmail.com>"
  :serial       t
  :license      "MIT"
  :depends-on   (#:bordeaux-threads #:cl-ppcre)
  :components   ((:module "src"
                          :components
                          ((:file "time")
                           (:file "clerk"))))
  :in-order-to ((test-op (test-op clerk-test))))

(defsystem #:clerk-test
  :description "A test system for clerk"
  :author "Petko Tsikov <tsikov@gmail.com>"
  :license "MIT"
  :depends-on (#:prove)
  :defsystem-depends-on (#:prove-asdf)
  :serial t
  :components ((:module "t"
                        :components
                        ((:test-file "clerk")
                         (:test-file "time"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
