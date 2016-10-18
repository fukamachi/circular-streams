# Circular-Streams - Circularly readable streams for Common Lisp

[![Build Status](https://travis-ci.org/fukamachi/circular-streams.svg?branch=master)](https://travis-ci.org/fukamachi/circular-streams)
[![Coverage Status](https://coveralls.io/repos/github/fukamachi/circular-streams/badge.svg?branch=master)](https://coveralls.io/github/fukamachi/circular-streams?branch=master)

## Usage

    (defparameter *stream*
                  (flex:make-in-memory-input-stream
                   #(72 101 108 108 111)))
    
    (defparameter *circular-stream*
                  (make-circular-stream *stream*))
    
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

Note this library can treat only octet streams which has a method `read-byte`. This might be expanded in the future.

This library was originally written by Tomohiro Matsuyama as a part of [Clack](http://clacklisp.org), Eitaro Fukamachi ported it with some improvements.

## API Reference

### [Class] circular-input-stream

Class for circular input streams. `make-circular-input-stream` is available to create an instance.

### [Function] make-circular-stream

Creates `circular-input-stream` and returns it.

## Author

* Tomohiro Matsuyama (tomo@cx4a.org)
* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011-2012 Tomohiro Matsuyama (tomo@cx4a.org)  
Copyright (c) 2012-2016 Eitaro Fukamachi (e.arrows@gmail.com)

# License

Licensed under the LLGPL License.

# See Also

* [Gray streams](http://www.cliki.net/Gray%20streams)
* [trivial-gray-streams](http://www.cliki.net/trivial-gray-streams)
