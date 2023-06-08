(defpackage #:plos
  (:use #:cl)
  (:shadow #:eval #:apply))
(in-package #:plos)

(defvar *heap*
  (sb-posix:mmap nil (* 4096 256) (logior sb-posix:prot-read sb-posix:prot-write)
                 (logior sb-posix:map-anon sb-posix:map-private) -1 0))

(defun make-cons-at-address (addr)
  (sb:%obj))
