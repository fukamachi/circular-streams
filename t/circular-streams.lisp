#|
  This file is a part of circular-streams project.
  Copyright (c) 2011-2012 Tomohiro Matsuyama (tomo@cx4a.org)
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage circular-streams-test
  (:use :cl
        :circular-streams
        :cl-test-more)
  (:import-from :flexi-streams
                :make-in-memory-input-stream
                :string-to-octets))
(in-package :circular-streams-test)

(plan 9)

(defparameter *stream*
              (flex:make-in-memory-input-stream
               (flex:string-to-octets "Hello")))

(defparameter *circular-stream*
              (make-circular-input-stream *stream*))

(is (read-char *circular-stream*) #\H)
(is (read-char *circular-stream*) #\e)
(is (read-char *circular-stream*) #\l)
(is (read-char *circular-stream*) #\l)
(is (read-char *circular-stream*) #\o)
(is (read-char *circular-stream* nil :eof) :eof)
(is (read-char *circular-stream*) #\H)

(let ((buf (make-array 5 :adjustable t :fill-pointer 5 :initial-element nil)))
  (read-sequence buf *circular-stream*)
  (is buf #(101 108 108 111 nil) :test #'equalp))

(let ((buf (make-array 5 :adjustable t :fill-pointer 5)))
  (read-sequence buf *circular-stream*)
  (is buf (flex:string-to-octets "Hello") :test #'equalp))

(finalize)
