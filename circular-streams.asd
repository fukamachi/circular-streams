#|
  Circularly readable streams for Common Lisp.

  Original Author: Tomohiro Matsuyama (tomo@cx4a.org)
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage circular-streams-asd
  (:use :cl :asdf))
(in-package :circular-streams-asd)

(defsystem circular-streams
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:fast-io
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
