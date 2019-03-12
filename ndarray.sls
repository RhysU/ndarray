;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2019 Rhys Ulerich
;; SPDX-License-Identifier: MIT
#!r6rs

(library (ndarray)
  (export hello)  ; TODO
  (import (rnrs))

; Akin to https://docs.scipy.org/doc/numpy/reference/arrays.dtypes.html
(define-record-type
  dtype
  (fields (immutable kind)
          (immutable itemsize)
          (immutable alignment)
          (immutable ref)
          (immutable set!))
  (opaque #t)
  (sealed #t))


; https://en.wikipedia.org/wiki/Dope_vector specified in units of itemsize
(define-record-type
  dope
  (fields (immutable shape)
          (immutable stride))
  (opaque #t)
  (sealed #t))

; Akin to https://docs.scipy.org/doc/numpy/reference/arrays.interface.html
(define-record-type
  ndarray
  (fields (immutable dtype)
          (immutable dope)
          (immutable offset)
          (immutable bytevector))
  (opaque #t)
  (sealed #t))

(define (hello whom)
  (string-append "Hello " whom "!")))
