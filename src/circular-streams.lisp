(in-package #:cl-user)
(defpackage #:circular-streams
  (:use #:cl
        #:trivial-gray-streams)
  (:import-from #:fast-io
                #:make-output-buffer
                #:fast-write-byte
                #:fast-write-sequence
                #:buffer-position
                #:output-buffer-queue
                #:output-buffer-vector
                #:finish-output-buffer)
  (:export #:circular-input-stream
           #:make-circular-stream
           #:make-circular-input-stream
           #:circular-stream-stream
           #:circular-stream-buffer
           #:circular-stream-position))
(in-package #:circular-streams)

(defclass circular-input-stream (trivial-gray-stream-mixin
                                 fundamental-binary-input-stream)
  ((stream :initarg :stream
           :reader circular-stream-stream)
   (buffer :initarg :buffer
           :initform (fast-io:make-output-buffer)
           :reader circular-stream-buffer)
   (position :initform 0
             :accessor circular-stream-position))
  (:documentation "Class for circular input streams."))

(defun make-circular-input-stream (stream &key (buffer (fast-io:make-output-buffer)))
  "Creates `circular-input-stream' and returns it. If `:buffer' is specified, the instance uses it as an internal buffer. You need this to reuse `circular-input-stream' without saving itself."
  (make-instance 'circular-input-stream
                 :stream stream
                 :buffer buffer))

(setf (fdefinition 'make-circular-stream) #'make-circular-input-stream)

(defun extend-buffer-one (stream)
  (let ((byte (read-byte (circular-stream-stream stream) nil :eof)))
    (unless (eq byte :eof)
      (fast-io:fast-write-byte byte (circular-stream-buffer stream)))
    byte))

(defun extend-buffer (stream count)
  (let* ((output-buffer (circular-stream-buffer stream))
         (buffer (make-array count :element-type '(unsigned-byte 8)))
         (buffer-end (read-sequence buffer (circular-stream-stream stream))))
    (fast-io:fast-write-sequence buffer
                                 output-buffer
                                 0 buffer-end)
    (values)))

(defmethod stream-element-type ((stream circular-input-stream))
  '(unsigned-byte 8))

(defun fast-buffer-aref (buffer index)
  (assert (< index (fast-io:buffer-position buffer)))
  (loop for buf in (fast-io::output-buffer-queue buffer)
        for i from 0
        if (< index (length buf))
          do (return (aref buf index))
        else
          do (decf index (length buf))
        finally
           (return (aref (fast-io::output-buffer-vector buffer) index))))

(defmethod stream-read-byte ((stream circular-input-stream))
  (with-slots (buffer position) stream
    (let ((byte
            (if (< position (fast-io:buffer-position buffer))
                (fast-buffer-aref buffer position)
                (extend-buffer-one stream))))
      (if (eq byte :eof)
          (setf position 0)
          (incf position))
      byte)))

(defmethod stream-read-char ((stream circular-input-stream))
  (let ((byte (stream-read-byte stream)))
    (if (eq byte :eof)
        byte
        (code-char byte))))

(defmethod stream-read-line ((stream circular-input-stream))
  (loop with buf = (make-string-output-stream :element-type 'character)
        for c = (read-char stream nil :eof)
        if (eq c :eof)
          return (values (get-output-stream-string buf) t)
        else if (char= c #\Newline)
          return (values (get-output-stream-string buf) nil)
        else
          do (write-char c buf)))

(defmethod stream-listen ((stream circular-input-stream))
  (with-slots (buffer position) stream
    (< position (fast-io:buffer-position buffer))))

(defmethod stream-read-sequence ((stream circular-input-stream) sequence start end &key)
  (with-slots (buffer position) stream
    (let ((to-read
            (- (+ position (- end start))
               (fast-io:buffer-position buffer))))
      (unless (<= to-read 0)
        (extend-buffer stream to-read))
      (let ((octets (fast-io:finish-output-buffer buffer)))
        (replace sequence octets
                 :start1 start
                 :end1 end
                 :start2 position)
        (let ((expected-end (- (min (+ (- end start) position)
                                    (length octets))
                               position)))
          (if (<= end expected-end)
              (progn
                (incf position expected-end)
                end)
              (progn
                (setf position 0)
                expected-end)))))))

(defmethod stream-file-position ((stream circular-input-stream))
  (circular-stream-position stream))

(defmethod (setf stream-file-position) (position-spec (stream circular-input-stream))
  (setf (circular-stream-position stream) position-spec))
