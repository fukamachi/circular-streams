#|
  This file is a part of circular-streams project.
  Copyright (c) 2011-2012 Tomohiro Matsuyama (tomo@cx4a.org)
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage circular-streams-test
  (:use :cl
        :circular-streams
        :trivial-gray-streams
        :cl-test-more))
(in-package :circular-streams-test)

(plan 10)

(defparameter *stream*
              (flex:make-in-memory-input-stream
               (flex:string-to-octets "Hello")))

(defparameter *circular-stream*
              (make-instance 'circular-stream
                 :stream *stream*))

(is (read-char *circular-stream*) #\H)
(is (read-char *circular-stream*) #\e)
(is (read-char *circular-stream*) #\l)
(is (read-char *circular-stream*) #\l)
(is (read-char *circular-stream*) #\o)
(is (read-char *circular-stream* nil :eof) :eof)
(is (read-char *circular-stream*) #\H)

(let ((buf (make-array 4 :adjustable t :fill-pointer 4)))
  (read-sequence buf *circular-stream*)
  (ok (equalp buf (flex:string-to-octets "ello"))))

(is (read-char *circular-stream* nil :eof) :eof)

(let ((buf (make-array 5 :adjustable t :fill-pointer 5)))
  (read-sequence buf *circular-stream*)
  (ok (equalp buf (flex:string-to-octets "Hello"))))

(finalize)
