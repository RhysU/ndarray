#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2019 Rhys Ulerich
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (ndarray))

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

; Confirm simple set/get works for various known dtypes
(for-each
  (lambda (under-test)
    (test-begin (string-append "read/write:" (dtype-descr under-test)))
    (let* ((dope (make-dope stride-c `(2 3)))
           (x (make-ndarray under-test dope)))
      (ndarray-set! x 19 0 2)
      (ndarray-set! x 31 1 2)
      (ndarray-set! x 29 1 1)
      (ndarray-set! x 23 1 0)
      (ndarray-set! x 17 0 1)
      (ndarray-set! x 13 0 0)
      (test-equal 17 (ndarray-ref x 0 1))
      (test-equal 29 (ndarray-ref x 1 1))
      (test-equal 31 (ndarray-ref x 1 2))
      (test-equal 13 (ndarray-ref x 0 0))
      (test-equal 23 (ndarray-ref x 1 0))
      (test-equal 19 (ndarray-ref x 0 2)))
    (test-end))
  (list
    dtype-s8 dtype-s16 dtype-s32 dtype-s64
    dtype-u8 dtype-u16 dtype-u32 dtype-u64))

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
