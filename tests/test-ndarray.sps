#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2019 Rhys Ulerich
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (ndarray))

; Column-major (aka C-order) stride computations
(test-begin "stride-c")
(test-equal `()       (stride-c `()))
(test-equal `(1)      (stride-c `(2)))
(test-equal `(3 1)    (stride-c `(2 3)))
(test-equal `(18 6 1) (stride-c `(2 3 6)))
(test-equal 0 (dope-index (make-dope stride-c `()) 0))
(test-equal 0 (dope-index (make-dope stride-c `(1)) 0 0))
(test-equal 4 (dope-index (make-dope stride-c `(5)) 0 4))
(test-equal 14 (dope-index (make-dope stride-c `(5)) 10 4))
(test-equal 1 (dope-index (make-dope stride-c `(3 5)) 0 0 1))
(test-equal 5 (dope-index (make-dope stride-c `(3 5)) 0 1 0))
(let ((x (make-ndarray dtype-s16 (make-dope stride-c `(2 3 6))) ))
  (test-equal (* 2 3 6 2) (bytevector-length (ndarray-bytevector x))))
(let ((x (make-ndarray dtype-s32 (make-dope stride-c `(2 3 6))) ))
  (test-equal (* 2 3 6 4) (bytevector-length (ndarray-bytevector x))))
(test-end)

; Row-major (aka Fortran-order) stride computations
(test-begin "stride-f")
(test-equal `()       (stride-f `()))
(test-equal `(1)      (stride-f `(2)))
(test-equal `(1 2)    (stride-f `(2 3)))
(test-equal `(1 2 6)  (stride-f `(2 3 6)))
(test-equal 0 (dope-index (make-dope stride-f `()) 0))
(test-equal 0 (dope-index (make-dope stride-f `(1)) 0 0))
(test-equal 4 (dope-index (make-dope stride-f `(5)) 0 4))
(test-equal 14 (dope-index (make-dope stride-f `(5)) 10 4))
(test-equal 3 (dope-index (make-dope stride-f `(3 5)) 0 0 1))
(test-equal 1 (dope-index (make-dope stride-f `(3 5)) 0 1 0))
(let ((x (make-ndarray dtype-s16 (make-dope stride-f `(2 3 6))) ))
  (test-equal (* 2 3 6 2) (bytevector-length (ndarray-bytevector x))))
(let ((x (make-ndarray dtype-s32 (make-dope stride-f `(2 3 6))) ))
  (test-equal (* 2 3 6 4) (bytevector-length (ndarray-bytevector x))))
(test-end)

; TODO Add tests for 1-dimensional ndarrays
; TODO Add tests for 3-dimensional ndarrays (likely with degenerate direction)
; TODO Zero-dimensional case works as expected?
; Confirm simple ref/set! works for various numeric dtypes and strides
(for-each
  (lambda (under-test)
    (test-begin (string-append "read/write:" (dtype-descr under-test)))
    (let ((c (make-ndarray under-test (make-dope stride-c `(2 3))))
          (f (make-ndarray under-test (make-dope stride-f `(2 3)))))
      (test-eq c c)
      (test-eq f f)
      (test-assert (not (eq? c f)))
      ; Setting positive indices
      (test-eq f (ndarray-set! f 31 +1 +2))
      (test-eq c (ndarray-set! c 19 +0 +2))
      (test-eq f (ndarray-set! f 13 +0 +0))
      (test-eq c (ndarray-set! c 29 +1 +1))
      (test-eq f (ndarray-set! f 17 +0 +1))
      (test-eq f (ndarray-set! f 19 +0 +2))
      ; Setting negative indices
      (test-eq c (ndarray-set! c 23 -1 -3))
      (test-eq c (ndarray-set! c 17 -2 -2))
      (test-eq f (ndarray-set! f 23 -1 -3))
      (test-eq c (ndarray-set! c 31 -1 -1))
      (test-eq f (ndarray-set! f 29 -1 -2))
      (test-eq c (ndarray-set! c 13 -2 -3))
      ; Retrieving positive indices
      (test-approximate 31 (ndarray-ref f +1 +2) 0)
      (test-approximate 23 (ndarray-ref f +1 +0) 0)
      (test-approximate 29 (ndarray-ref c +1 +1) 0)
      (test-approximate 17 (ndarray-ref f +0 +1) 0)
      (test-approximate 31 (ndarray-ref c +1 +2) 0)
      (test-approximate 17 (ndarray-ref c +0 +1) 0)
      (test-approximate 29 (ndarray-ref f +1 +1) 0)
      (test-approximate 13 (ndarray-ref f +0 +0) 0)
      (test-approximate 23 (ndarray-ref c +1 +0) 0)
      (test-approximate 13 (ndarray-ref c +0 +0) 0)
      (test-approximate 19 (ndarray-ref c +0 +2) 0)
      (test-approximate 19 (ndarray-ref f +0 +2) 0)
      ;; Retrieving negative indices
      (test-approximate 31 (ndarray-ref f -1 -1) 0)
      (test-approximate 23 (ndarray-ref f -1 -3) 0)
      (test-approximate 29 (ndarray-ref c -1 -2) 0)
      (test-approximate 17 (ndarray-ref f -2 -2) 0)
      (test-approximate 31 (ndarray-ref c -1 -1) 0)
      (test-approximate 17 (ndarray-ref c -2 -2) 0)
      (test-approximate 29 (ndarray-ref f -1 -2) 0)
      (test-approximate 13 (ndarray-ref f -2 -3) 0)
      (test-approximate 23 (ndarray-ref c -1 -3) 0)
      (test-approximate 13 (ndarray-ref c -2 -3) 0)
      (test-approximate 19 (ndarray-ref c -2 -1) 0)
      (test-approximate 19 (ndarray-ref f -2 -1) 0))
    (test-end))
  (list
    dtype-s8 dtype-s16 dtype-s32 dtype-s64
    dtype-u8 dtype-u16 dtype-u32 dtype-u64
    dtype-f32 dtype-f64))

; Slice creation with various numbers of defaults
(test-begin "make-slice")
(let ((a (make-slice)))
    (test-assert (not (slice-start a)))
    (test-assert (not (slice-stop a)))
    (test-assert (not (slice-step a))))
(let ((a (make-slice 5)))
    (test-equal 5 (slice-start a))
    (test-assert (not (slice-stop a)))
    (test-assert (not (slice-step a))))
(let ((a (make-slice 5 6)))
    (test-equal 5 (slice-start a))
    (test-equal 6 (slice-stop a))
    (test-assert (not (slice-step a))))
(let ((a (make-slice 5 6 7)))
    (test-equal 5 (slice-start a))
    (test-equal 6 (slice-stop a))
    (test-equal 7 (slice-step a)))
(test-end)

; Sliver extents given various slicing options
(test-begin "sliver-extent")
; Default start/stop has step 1
(test-equal 4 (sliver-extent (make-sliver (make-slice) 4)))
(test-equal 5 (sliver-extent (make-sliver (make-slice) 5)))
; Explicit start/stop has step 1
(test-equal 4 (sliver-extent (make-sliver (make-slice #f) 4)))
(test-equal 5 (sliver-extent (make-sliver (make-slice #f) 5)))
; Default stop is the extent
(test-equal 0 (sliver-extent (make-sliver (make-slice 5) 4)))
(test-equal 0 (sliver-extent (make-sliver (make-slice 5) 5)))
(test-equal 1 (sliver-extent (make-sliver (make-slice 5) 6)))
(test-equal 2 (sliver-extent (make-sliver (make-slice 5) 7)))
(test-equal 3 (sliver-extent (make-sliver (make-slice 5) 8)))
(test-equal 4 (sliver-extent (make-sliver (make-slice 5) 9)))
; Explicit stop is the extent
(test-equal 0 (sliver-extent (make-sliver (make-slice 5 #f) 4)))
(test-equal 0 (sliver-extent (make-sliver (make-slice 5 #f) 5)))
(test-equal 1 (sliver-extent (make-sliver (make-slice 5 #f) 6)))
(test-equal 2 (sliver-extent (make-sliver (make-slice 5 #f) 7)))
(test-equal 3 (sliver-extent (make-sliver (make-slice 5 #f) 8)))
(test-equal 4 (sliver-extent (make-sliver (make-slice 5 #f) 9)))
; Default step of 1
(test-equal 0 (sliver-extent (make-sliver (make-slice 5 8) 4)))
(test-equal 0 (sliver-extent (make-sliver (make-slice 5 8) 5)))
(test-equal 1 (sliver-extent (make-sliver (make-slice 5 8) 6)))
(test-equal 2 (sliver-extent (make-sliver (make-slice 5 8) 7)))
(test-equal 3 (sliver-extent (make-sliver (make-slice 5 8) 8)))
(test-equal 3 (sliver-extent (make-sliver (make-slice 5 8) 9)))
; Explicit step of 1
(test-equal 0 (sliver-extent (make-sliver (make-slice 5 8 1) 4)))
(test-equal 0 (sliver-extent (make-sliver (make-slice 5 8 1) 5)))
(test-equal 1 (sliver-extent (make-sliver (make-slice 5 8 1) 6)))
(test-equal 2 (sliver-extent (make-sliver (make-slice 5 8 1) 7)))
(test-equal 3 (sliver-extent (make-sliver (make-slice 5 8 1) 8)))
(test-equal 3 (sliver-extent (make-sliver (make-slice 5 8 1) 9)))
; Explicit step of 2
(test-equal 0 (sliver-extent (make-sliver (make-slice 5 8 2) 4)))
(test-equal 0 (sliver-extent (make-sliver (make-slice 5 8 2) 5)))
(test-equal 1 (sliver-extent (make-sliver (make-slice 5 8 2) 6)))
(test-equal 1 (sliver-extent (make-sliver (make-slice 5 8 2) 7)))
(test-equal 2 (sliver-extent (make-sliver (make-slice 5 8 2) 8)))
(test-equal 2 (sliver-extent (make-sliver (make-slice 5 8 2) 9)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
