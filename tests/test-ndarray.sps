#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2019 Rhys Ulerich
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (ndarray))

(test-begin "stride-c")
(test-equal `()        (stride-c 1 `()))
(test-equal `(1)       (stride-c 1 `(2)))
(test-equal `(3 1)     (stride-c 1 `(2 3)))
(test-equal `(18  6 1) (stride-c 1 `(2 3 6)))
(test-equal `(36 12 2) (stride-c 2 `(2 3 6)))
(test-end)

(test-begin "stride-f")
(test-equal `()       (stride-f 1 `()))
(test-equal `(1)      (stride-f 1 `(2)))
(test-equal `(1 2)    (stride-f 1 `(2 3)))
(test-equal `(1 2 6)  (stride-f 1 `(2 3 6)))
(test-equal `(2 4 12) (stride-f 2 `(2 3 6)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
