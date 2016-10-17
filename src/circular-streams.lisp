#|
  This file is a part of circular-streams project.
  Copyright (c) 2011-2012 Tomohiro Matsuyama (tomo@cx4a.org)
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage circular-streams
  (:use :cl
        :trivial-gray-streams)
  (:export :circular-stream-stream
           :circular-stream-buffer
           :circular-stream-position))
(in-package :circular-streams)

(cl-syntax:use-syntax :annot)

@export
(defun make-circular-stream-buffer ()
  "Creates a buffer array for `circular-input-stream'."
  (make-array 0
              :element-type 'octet
              :adjustable t
              :fill-pointer 0))

@export
(defclass circular-input-stream (trivial-gray-stream-mixin
                                 fundamental-binary-input-stream)
     ((stream :initarg :stream
              :reader circular-stream-stream)
      (buffer :initarg :buffer
              :initform (make-circular-stream-buffer)
              :reader circular-stream-buffer)
      (position :initform 0
                :accessor circular-stream-position))
  (:documentation "Class for circular input streams. `make-circular-input-stream' is available to create an instance."))

@export
(defun make-circular-input-stream (stream &key (buffer (make-circular-stream-buffer)))
  "Creates `circular-input-stream' and returns it. If `:buffer' is specified, the instance uses it as an internal buffer. You need this to reuse `circular-input-stream' without saving itself."
  (make-instance 'circular-input-stream
     :stream stream
     :buffer buffer))

(deftype octet () '(unsigned-byte 8))

(defmethod stream-element-type ((this circular-input-stream))
  'octet)

(defmethod circular-stream-fetch ((this circular-input-stream))
  (with-slots (stream buffer position) this
     (let ((to-be-fetched (1+ (- position (length buffer)))))
       ;; TODO use read-sequence
       (loop for fetched from 0 below to-be-fetched
             for octet = (read-byte stream nil nil)
             if octet
               do (vector-push-extend octet buffer)
             else
               return fetched
             finally (return fetched)))))

(defmethod stream-read-byte ((this circular-input-stream))
  (with-slots (stream buffer position) this
     (if (or (< position (length buffer))
             (plusp (circular-stream-fetch this)))
         (prog1
           (aref buffer position)
           (incf position))
         (prog1
           :eof
           (setf position 0)))))

(defmethod stream-read-char ((this circular-input-stream))
  (let ((byte (stream-read-byte this)))
    (if (eq byte :eof)
        byte
        (code-char byte))))

(defmethod stream-read-line ((this circular-input-stream))
  (loop with buf = (make-string-output-stream :element-type 'character)
        for c = (read-char this nil :eof)
        if (eq c :eof)
          return (values (get-output-stream-string buf) t)
        else if (char= c #\Newline)
          return (values (get-output-stream-string buf) nil)
        else
          do (write-char c buf)))

(defmethod stream-listen ((this circular-input-stream))
  (with-slots (buffer position) this
     (< position (length buffer))))

(defmethod stream-read-sequence ((this circular-input-stream) sequence start end &key)
  (loop for index from start below end
        for octet = (stream-read-byte this)
        while (typep octet 'octet)
        do (setf (elt sequence index) octet)
        finally (return index)))

(defmethod stream-file-position ((this circular-input-stream))
  (circular-stream-position this))

(defmethod (setf stream-file-position) (position-spec (this circular-input-stream))
  (setf (circular-stream-position this) position-spec))
