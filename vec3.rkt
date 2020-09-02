#lang racket

(provide (all-defined-out))

(require racket/performance-hint)

(struct vec3 (x y z)
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "~a ~a ~a" (vec3-x v) (vec3-y v) (vec3-z v)))]
  #:transparent
  #:mutable)
(struct color vec3 ()
  #:methods gen:custom-write
  [(define (write-proc c port mode)
     (fprintf port "~a ~a ~a~n"
              (inexact->exact (truncate (* 255.99 (vec3-x c))))
              (inexact->exact (truncate (* 255.99 (vec3-y c))))
              (inexact->exact (truncate (* 255.99 (vec3-z c))))))]
  #:transparent)
(struct point3 vec3 () #:transparent)

(define (vec3-length-squared v)
  (match v
    [(vec3 x y z) (+ (* x x) (* y y) (* z z))]))
(define (vec3-length v)
  (sqrt (vec3-length-squared v)))

(define (vec3-neg v)
  (match v
    [(vec3 x y z)
     (vec3 (- x) (- y) (- z))]))

(define (vec3-+= v v1)
  (match* (v v1)
    [((vec3 x y z) (vec3 x1 y1 z1))
     (set-vec3-x! v (+ x x1))
     (set-vec3-y! v (+ y y1))
     (set-vec3-z! v (+ z z1))]))
(define (vec3-*= v t)
  (match v
    [(vec3 x y z)
     (set-vec3-x! v (* x t))
     (set-vec3-y! v (* y t))
     (set-vec3-z! v (* z t))]))
(define (vec3-/= v t)
  (vec3-*= v (/ 1 t)))

(define-inline (vec3-+ u v)
  (match* (u v)
    [((vec3 x y z) (vec3 x1 y1 z1))
     (vec3 (+ x x1) (+ y y1) (+ z z1))]))
(define-inline (vec3-- u v)
  (match* (u v)
    [((vec3 x y z) (vec3 x1 y1 z1))
     (vec3 (- x x1) (- y y1) (- z z1))]))
(define-inline (vec3-* u v)
  (match* (u v)
    [((vec3 x y z) (vec3 x1 y1 z1))
     (vec3 (* x x1) (* y y1) (* z z1))]
    [((vec3 x y z) (? number?))
     (vec3 (* x u) (* y u) (* z u))]
    [((? number?) (vec3 x y z)) (vec3-* v u)]))
(define-inline (vec3-/ v t)
  (vec3-* (1 . / . t) v))
(define-inline (dot u v)
  (match* (u v)
    [((vec3 x y z) (vec3 x1 y1 z1))
     (+ (* x x1) (* y y1) (* z z1))]))
(define-inline (cross u v)
  (match* (u v)
    [((vec3 x y z) (vec3 x1 y1 z1))
     (vec3 (- (* y z1) (* z y1))
           (- (* z x1) (* x z1))
           (- (* x y1) (* y x1)))]))
(define-inline (unit-vector v)
  (vec3-/ v (vec3-length v)))
