# Circular-Streams - Circulus streams for Common Lisp.

## Usage

    (defparameter *stream*
                  (flex:make-in-memory-input-stream
                   #(72 101 108 108 111)))
    
    (defparameter *circular-stream*
                  (make-instance 'circular-stream
                     :stream *stream*))
    
    (is (read-char *circular-stream*) #\H)
    (is (read-char *circular-stream*) #\e)
    (is (read-char *circular-stream*) #\l)
    (is (read-char *circular-stream*) #\l)
    (is (read-char *circular-stream*) #\o)
    (is (read-char *circular-stream* nil :eof) :eof)
    
    (let ((buf (make-array 5 :adjustable t :fill-pointer 5)))
      (read-sequence buf *circular-stream*)
      (flex:octets-to-string buf))
    ;=> "Hello"

## Installation

## Author

* Tomohiro Matsuyama (tomo@cx4a.org)
* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011-2012 Tomohiro Matsuyama (tomo@cx4a.org)
Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the LLGPL License.
