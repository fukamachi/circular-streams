(in-package :cl-user)
(defpackage circular-streams-test-asd
  (:use :cl :asdf))
(in-package :circular-streams-test-asd)

(defsystem circular-streams-test
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:circular-streams
               :cl-test-more
               :flexi-streams)
  :components ((:module "t"
                :components
                ((:file "circular-streams"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
