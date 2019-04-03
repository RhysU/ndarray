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
(make-ndarray dtype-s16 (make-dope stride-c `(2 3 6)))
(make-ndarray dtype-s32 (make-dope stride-c `(2 3 6)))
(make-ndarray dtype-s64 (make-dope stride-c `(2 3 6)))
(test-end)

(test-begin "stride-f")
(test-equal `()       (stride-f `()))
(test-equal `(1)      (stride-f `(2)))
(test-equal `(1 2)    (stride-f `(2 3)))
(test-equal `(1 2 6)  (stride-f `(2 3 6)))
(make-ndarray dtype-s16 (make-dope stride-f `(2 3 6)))
(make-ndarray dtype-s32 (make-dope stride-f `(2 3 6)))
(make-ndarray dtype-s64 (make-dope stride-f `(2 3 6)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
