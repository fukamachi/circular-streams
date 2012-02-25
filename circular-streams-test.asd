#|
  This file is a part of circular-streams project.
  Copyright (c) 2011-2012 Tomohiro Matsuyama (tomo@cx4a.org)
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage circular-streams-test-asd
  (:use :cl :asdf))
(in-package :circular-streams-test-asd)

(defsystem circular-streams-test
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:circular-streams
               :cl-test-more
               :flexi-streams)
  :components ((:module "t"
                :components
                ((:file "circular-streams"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
