;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2019 Rhys Ulerich
;; SPDX-License-Identifier: MIT
#!r6rs

(library (ndarray)
  (export stride-c stride-f)
  (import (rnrs))

; FIXME Generative or not?

; Akin to https://docs.scipy.org/doc/numpy/reference/arrays.dtypes.html
; In particular, 'descr' follows NumPy conventions.
(define-record-type
  dtype
  (fields (immutable descr)
          (immutable itemsize)
          (immutable alignment)
          (immutable ref)
          (immutable set!))
  (opaque #f)
  (sealed #t))

; Single bytes
(define dtype-s8
  (make-dtype "|i1" 1 1 bytevector-s8-ref bytevector-s8-set!))
(define dtype-u8
  (make-dtype "|u1" 1 1 bytevector-u8-ref bytevector-u8-set!))

; Signed integers
(define dtype-s16
  (make-dtype "=i2" 2 2 bytevector-s16-native-ref bytevector-s16-native-set!))
(define dtype-s32
  (make-dtype "=i4" 4 4 bytevector-s32-native-ref bytevector-s32-native-set!))
(define dtype-s64
  (make-dtype "=i8" 8 8 bytevector-s64-native-ref bytevector-s64-native-set!))

; Unsigned integers
(define dtype-u16
  (make-dtype "=u2" 2 2 bytevector-u16-native-ref bytevector-u16-native-set!))
(define dtype-u32
  (make-dtype "=u4" 4 4 bytevector-u32-native-ref bytevector-u32-native-set!))
(define dtype-u64
  (make-dtype "=u8" 8 8 bytevector-u64-native-ref bytevector-u64-native-set!))

; Single- and double-precision floating point
(define dtype-f32
  (make-dtype "=f4" 4 4
              bytevector-ieee-single-native-ref
              bytevector-ieee-single-native-set!))
(define dtype-f64
  (make-dtype "=f8" 8 8
              bytevector-ieee-double-native-ref
              bytevector-ieee-double-native-set!))

; https://en.wikipedia.org/wiki/Dope_vector specified in units of itemsize
(define-record-type
  (dope make-dope* dope?)
  (fields (immutable stride)
          (immutable shape))
  (opaque #f)
  (sealed #t))

; Compute "C" (i.e. row-major) strides for contiguous shape
(define (stride-c shape)
  (assert (list? shape))
  (let loop ((m 1) (xs (reverse shape)) (as `()))
    (if (pair? xs)
      (loop (* m (car xs)) (cdr xs) (cons m as))
      as)))

; Compute "F" (i.e. column-major) strides for contiguous shape
(define (stride-f shape)
  (assert (list? shape))
  (let loop ((m 1) (xs shape) (as `()))
    (if (pair? xs)
      (loop (* m (car xs)) (cdr xs) (cons m as))
      (reverse as))))

; Supply striding (list or unary procedure list stride-c) and shape
(define (make-dope striding shape)
  (let [(stride* (list->vector (cond ((procedure? striding) (striding shape))
                                     (else striding))))
        (shape* (list->vector shape))]
    (assert (= (vector-length shape*) (vector-length stride*)))
    (make-dope* shape* stride*)))

; A limited fold for pairwise reduction of vectors v1 and v2.
; Cleaner alternative is adopting SRFI 43 but eschewing dependency for now.
(define (vector-fold2 combine nil vector1 vector2)
  (let ((length1 (vector-length vector1))
        (length2 (vector-length vector2)))
    (assert (= length1 length2))
    (let loop ((i 0) (cur nil))
      (if (< i length1)
        (loop
          (+ i 1)
          (combine cur (vector-ref vector1 i) (vector-ref vector2 i)))
        cur))))

; Compute contiguous size, measured in items, necessary to store dope
(define (size dope)
  (vector-fold2
    (lambda (cur strideN shapeN) (max cur (* strideN shapeN)))
    0
    (dope-stride dope)
    (dope-shape dope)))

; Akin to https://docs.scipy.org/doc/numpy/reference/arrays.interface.html
(define-record-type
  (ndarray make-ndarray* ndarray?)
  (fields (immutable dtype)
          (immutable dope)
          (immutable offset)
          (immutable bytevector))
  (opaque #f)
  (sealed #t))

; Construct an un-initialized ndarray with the given dtype and dope
(define (make-ndarray dtype dope)
  (make-ndarray
    dtype
    dope
    0
    (make-bytevector (* (size dope) (dtype-itemsize dtype)))))

; TODO Generic getter
; TODO Generic setter
; TODO Mutable subsets of existing ndarrays
)
