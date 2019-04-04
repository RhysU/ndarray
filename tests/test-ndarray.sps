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

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
