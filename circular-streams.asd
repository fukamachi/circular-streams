#|
  This file is a part of circular-streams project.
  Copyright (c) 2011-2012 Tomohiro Matsuyama (tomo@cx4a.org)
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Circularly readable streams for Common Lisp.

  Original Author: Tomohiro Matsuyama (tomo@cx4a.org)
  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage circular-streams-asd
  (:use :cl :asdf))
(in-package :circular-streams-asd)

(defsystem circular-streams
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:cl-annot
               :cl-syntax-annot
               :trivial-gray-streams)
  :components ((:module "src"
                :components
                ((:file "circular-streams"))))
  :description "Circularly readable streams for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op circular-streams-test))))
