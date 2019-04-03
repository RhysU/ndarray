;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2019 Rhys Ulerich
;; SPDX-License-Identifier: MIT
#!r6rs

(library (ndarray)
  (export
    dtype-s8 dtype-s16 dtype-s32 dtype-s64
    dtype-u8 dtype-u16 dtype-u32 dtype-u64
    stride-c stride-f
    make-dope dope? dope-stride dope-shape
    make-ndarray ndarray? #|more...|#)
  (import (rnrs))

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
  (sealed #t)
  (nongenerative))

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
  (sealed #t)
  (nongenerative))

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
    (make-dope* stride* shape*)))

; Compute contiguous size, measured in items, necessary to store dope
(define (size dope)
  (assert (dope? dope))
  (let ((size 0))
    (vector-for-each
      (lambda (x y) (set! size (max size (* x y))))
      (dope-stride dope)
      (dope-shape dope))
    size))

; Scale strides by some given itemsize.  For example, stride over floats.
(define (scale dope itemsize)
  (assert (dope? dope))
  (assert (number? itemsize))
  (make-dope*
    (vector-map
      (lambda (x) (* itemsize x))
      (dope-stride dope))
    (dope-shape dope)))

; Akin to https://docs.scipy.org/doc/numpy/reference/arrays.interface.html
(define-record-type
  (ndarray make-ndarray* ndarray?)
  (fields (immutable dtype)
          (immutable dope)
          (immutable offset)
          (immutable bytevector))
  (opaque #f)
  (sealed #t)
  (nongenerative))

; Construct an uninitialized ndarray with the given dtype and dope
(define (make-ndarray dtype dope)
  (let ((itemsize (dtype-itemsize dtype)))
    (make-ndarray*
      dtype
      (scale dope itemsize)
      0
      (make-bytevector (* (size dope) itemsize)))))

; TODO Generic getter
; TODO Generic setter
; TODO Mutable subsets of existing ndarrays
)
