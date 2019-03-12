;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2019 Rhys Ulerich
;; SPDX-License-Identifier: MIT
#!r6rs

(library (ndarray)
  (export hello)
  (import (rnrs))

(define-record-type
  ndarray
  (opaque #t)
  (fields (immutable typekind)
          (immutable itemsize)
          (immutable flags)
          (immutable shape)
          (immutable strides)
          (immutable offset)
          (immutable bytevector)))

(define (hello whom)
  (string-append "Hello " whom "!")))
