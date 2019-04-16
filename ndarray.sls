;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2019 Rhys Ulerich
;; SPDX-License-Identifier: MIT
#!r6rs

; TODO Revisit assert vs error handling
(library (ndarray)
  (export
    make-dtype dtype?
    dtype-descr dtype-itemsize dtype-alignment dtype-ref dtype-set!
    dtype-s8 dtype-s16 dtype-s32 dtype-s64
    dtype-u8 dtype-u16 dtype-u32 dtype-u64
    dtype-f32 dtype-f64
    stride-c stride-f
    make-dope dope? dope-stride dope-shape
    dope-size dope-scale dope-index
    make-ndarray ndarray?
    ndarray-dtype ndarray-dope ndarray-offset ndarray-bytevector
    ndarray-ref ndarray-set!
    slice make-slice slice-start* slice-stop* slice-step*
    slice->list slice->vector)
  (import (rnrs))

; Akin to https://docs.scipy.org/doc/numpy/reference/arrays.dtypes.html
; In particular, 'descr' follows NumPy conventions.
(define-record-type
  (dtype make-dtype dtype?)
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
  (let loop ((m 1) (xs (reverse shape)) (as `()))
    (if (pair? xs)
      (loop (* m (car xs)) (cdr xs) (cons m as))
      as)))

; Compute "F" (i.e. column-major) strides for contiguous shape
(define (stride-f shape)
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

; What is the rank of a dope vector?  That is, the number of dimensions.
(define (dope-rank dope)
  (vector-length (dope-shape dope)))

; Compute contiguous size, measured in items, necessary to store dope
(define (dope-size dope)
  (let ((size 0))
    (vector-for-each
      (lambda (x y) (set! size (max size (* x y))))
      (dope-stride dope)
      (dope-shape dope))
    size))

; Scale strides by some given itemsize.  For example, stride over floats.
(define (dope-scale dope itemsize)
  (make-dope*
    (vector-map
      (lambda (x) (* itemsize x))
      (dope-stride dope))
    (dope-shape dope)))

; Compute offset for some (i, j, ...) based upon dope-vector strides.
; Allows NumPy-style negative indices, where i < 0 selects i + extent.
(define (dope-index dope offset . indices)
  (let ((stride (dope-stride dope))
        (shape (dope-shape dope)))
    (let loop ((k 0)
               (indices indices)
               (offset offset))
      (if (null? indices)
        (begin
          (assert (= k (vector-length stride)))
          offset)
        (let ((extent (vector-ref shape k))
              (i (car indices)))
          (if (>= i 0)
            (assert (< i extent))
            (begin
              (assert (>= i (- extent)))
              (set! i (+ i extent))))
          (loop (+ k 1)
                (cdr indices)
                (+ offset (* i (vector-ref stride k)))))))))

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
  (assert (dtype? dtype))
  (let* ((itemsize (dtype-itemsize dtype))
         (rescaled (dope-scale dope itemsize)))
    (make-ndarray* dtype rescaled 0 (make-bytevector (dope-size rescaled)))))

; Retrieve the scalar value at the given index
(define (ndarray-ref ndarray . indices)
  ((dtype-ref
     (ndarray-dtype ndarray))
   (ndarray-bytevector ndarray)
   (apply
     dope-index
     (ndarray-dope ndarray)
     (ndarray-offset ndarray)
     indices)))

; Set the given index to the provided scalar value *returning the ndarray*
(define (ndarray-set! ndarray scalar . indices)
  ((dtype-set!
     (ndarray-dtype ndarray))
   (ndarray-bytevector ndarray)
   (apply
     dope-index
     (ndarray-dope ndarray)
     (ndarray-offset ndarray)
     indices)
   scalar)
  ndarray)

; Akin to https://docs.python.org/3.6/library/functions.html#slice,
; with #f used where Python would choose None.  Value stop is exclusive.
(define-record-type
  (slice make-slice* slice?)
  (fields (immutable start slice-start*)
          (immutable stop slice-stop*)
          (immutable step slice-step*))
  (opaque #f)
  (sealed #t)
  (nongenerative))

(define (slice->list slice)
  (let ((x (make-vector 3)))
    (vector-set! x 0 (slice-start* slice))
    (vector-set! x 1 (slice-stop* slice))
    (vector-set! x 2 (slice-step* slice))
    x))

(define (slice->vector slice)
  (list
    (slice-start* slice)
    (slice-stop* slice)
    (slice-step* slice)))

; Construct slices with various settings
(define make-slice
  (let ((start-or-stop? (lambda (x) (or (number? x) (eq? x #f)))))
    (case-lambda
      (()
       (make-slice* #f #f 1))
      ((start)
       (assert (start-or-stop? start))
       (make-slice* start #f 1))
      ((start stop)
       (assert (start-or-stop? start))
       (assert (start-or-stop? stop))
       (make-slice* start stop 1))
      ((start stop step)
       (assert (start-or-stop? start))
       (assert (start-or-stop? stop))
       (assert (number? step))
       (make-slice* start stop step)))))

; TODO Testing very much remains
; Replace #f or negative indices with concrete values relative to extent.
; Must be wholistic across start/stop/step as some default have cross-talk.
; Follows https://docs.scipy.org/doc/numpy/reference/arrays.indexing.html.
(define (slice-normalize slice extent)
  (let ((start (slice-start* slice))
        (stop (slice-stop* slice))
        (step (slice-step* slice)))
    (make-slice*
      (cond
        ((not start)
         (if (positive? step) 0 (- extent 1)))
        ((negative? start)
         (max 0 (+ extent start)))
        (else start))
      (cond
        ((not stop)
         (if (positive? step) extent 0))
        ((negative? stop)
         (min extent (+ extent stop)))
        (else stop))
      step)))

; TODO Mutable subsets of existing ndarrays
; TODO ndarray-copy
; TODO ndarray-fill!
; TODO ndarray-flatten
; TODO ndarray-like
; TODO ndarray-ravel
; TODO ndarray-repeat
; TODO ndarray-reshape
; TODO ndarray-resize
; TODO ndarray-squeeze
; TODO ndarray-swapaxes
; TODO ndarray-transpose
; TODO ndarray-view
; TODO Printed output
)
