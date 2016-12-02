(in-package :cl-user)
(defpackage circular-streams-test
  (:use :cl
        :circular-streams
        :cl-test-more)
  (:import-from :flexi-streams
                :make-in-memory-input-stream
                :string-to-octets))
(in-package :circular-streams-test)

(plan 15)

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

(file-position *circular-stream* 0)
(let ((buf (make-array 2 :element-type '(unsigned-byte 8))))
  (is (read-sequence buf *circular-stream*) 2)
  (is (flex:octets-to-string buf) "He" :test #'equalp)
  (is (read-sequence buf *circular-stream*) 2)
  (is (flex:octets-to-string buf) "ll" :test #'equalp)
  (is (read-sequence buf *circular-stream*) 1)
  (is (flex:octets-to-string buf :end 1) "o" :test #'equalp))

(finalize)
