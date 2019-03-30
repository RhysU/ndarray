;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2019 Rhys Ulerich
;; SPDX-License-Identifier: MIT
#!r6rs

(library (ndarray)
  (export stride-c stride-f)
  (import (rnrs))

; TODO dtype-const is a parent of dtype simply lacking set!

; Akin to https://docs.scipy.org/doc/numpy/reference/arrays.dtypes.html
; In particular, 'descr' follows NumPy conventions.
(define-record-type
  dtype
  (fields (immutable descr)
          (immutable itemsize)
          (immutable alignment)
          (immutable ref)
          (immutable set!))
  (opaque #t)
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
  (fields (immutable shape)
          (immutable stride))
  (opaque #t)
  (sealed #t))

; Shape and stride taken as lists then copied into vectors
(define (make-dope shape stride)
  (let ((shape* (list->vector shape))
        (stride* (list->vector stride)))
    (assert (= (vector-length shape*) (vector-length stride*)))
    (make-dope* shape* stride*)))

; Compute "C" (i.e. row-major) strides for given shape
(define (stride-c shape)
  (if (null? shape)
    `()
    (fold-left (lambda (as b) (cons (* b (car as)) as))
               `(1)
               (reverse (cdr shape)))))

; Compute "F" (i.e. column-major) strides for given shape
(define (stride-f shape)
  (if (null? shape)
    `()
    (reverse (cdr (fold-left (lambda (as b) (cons (* b (car as)) as))
                             `(1)
                             shape)))))

; Akin to https://docs.scipy.org/doc/numpy/reference/arrays.interface.html
(define-record-type
  (ndarray make-ndarray* ndarray?)
  (fields (immutable dtype)
          (immutable dope)
          (immutable offset)
          (immutable bytevector))
  (opaque #t)
  (sealed #t))

; TODO Create row-major ndarray given dtype and shape
; TODO Create column-major ndarray given dtype and shape
; TODO Generic getter
; TODO Generic setter
; TODO Mutable views into existing ndarrays
; TODO Immutable views into existing ndarrays
)
