# Circular-Streams - Circulus streams for Common Lisp.

## Usage

    (defparameter *stream*
                  (flex:make-in-memory-input-stream
                   #(72 101 108 108 111)))
    
    (defparameter *circular-stream*
                  (make-instance 'circular-stream
                     :stream *stream*))
    
    (read-char *circular-stream*)          ;=> #\H
    (read-char *circular-stream*)          ;=> #\e
    (read-char *circular-stream*)          ;=> #\l
    (read-char *circular-stream*)          ;=> #\l
    (read-char *circular-stream*)          ;=> #\o
    (read-char *circular-stream* nil :eof) ;=> :eof
    
    (let ((buf (make-array 5 :adjustable t :fill-pointer 5)))
      (read-sequence buf *circular-stream*)
      (flex:octets-to-string buf))
    ;=> "Hello"

## Description

Circular-Streams allows you to read streams circularly by wrapping real streams. Once you reach end-of-file of a stream, it's file position will be reset to 0 and you're able to read it again.

This library was originally written by Tomohiro Matsuyama as a part of [Clack](http://clacklisp.org), Eitarow Fukamachi ported it with some improvements.

## Author

* Tomohiro Matsuyama (tomo@cx4a.org)
* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011-2012 Tomohiro Matsuyama (tomo@cx4a.org)  
Copyright (c) 2012 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the LLGPL License.

# See Also

* [Gray streams](http://www.cliki.net/Gray%20streams)
* [trivial-gray-streams](http://www.cliki.net/trivial-gray-streams)
