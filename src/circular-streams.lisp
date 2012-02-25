#|
  This file is a part of circular-streams project.
  Copyright (c) 2011-2012 Tomohiro Matsuyama (tomo@cx4a.org)
  Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage circular-streams
  (:use :cl
        :trivial-gray-streams))
(in-package :circular-streams)

(cl-syntax:use-syntax :annot)

@export
(defun make-buffer ()
  (make-array 0
              :element-type 'octet
              :adjustable t
              :fill-pointer 0))

@export
(defclass circular-stream (trivial-gray-stream-mixin
                           fundamental-binary-input-stream)
     ((stream :initarg :stream
              :reader circular-stream-stream)
      (buffer :initarg :buffer
              :initform (make-buffer)
              :reader circular-stream-buffer)
      (position :initform 0
                :accessor circular-stream-position)))

(deftype octet () '(unsigned-byte 8))

(defmethod stream-element-type ((this circular-stream))
  'octet)

@export
(defmethod circular-stream-fetch ((this circular-stream))
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

(defmethod stream-read-byte ((this circular-stream))
  (with-slots (stream buffer position) this
     (if (or (< position (length buffer))
             (plusp (circular-stream-fetch this)))
         (prog1
           (aref buffer position)
           (incf position))
         (prog1
           :eof
           (setf (circular-stream-position this) 0)))))

(defmethod stream-read-char ((this circular-stream))
  (let ((byte (stream-read-byte this)))
    (if (eq byte :eof)
        byte
        (code-char byte))))

(defmethod stream-listen ((this circular-stream))
  (with-slots (buffer position) this
     (< position (length buffer))))

(defmethod stream-read-sequence ((this circular-stream) sequence start end &key)
  (loop for index from start below end
        for octet = (stream-read-byte this)
        while (typep octet 'octet)
        do (setf (elt sequence index) octet)
        finally (return index)))

(defmethod stream-file-position ((this circular-stream))
  (circular-stream-position this))

(defmethod (setf stream-file-position) (position-spec (this circular-stream))
  (setf (circular-stream-position this) position-spec))
