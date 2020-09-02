#lang racket

(struct vec3 (x y z)
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "~a ~a ~a" (vec3-x v) (vec3-y v) (vec3-z v)))]
  #:transparent
  #:mutable)
(define point3 vec3)
(define color vec3)

(define (vec-length-squared v)
  (match v
    [(vec3 x y z) (+ (* x x) (* y y) (* z z))]))
(define (vec-length v)
  (sqrt (vec-length-squared v)))

(define (vec-neg v)
  (match v
    [(vec3 x y z)
     (vec3 (- x) (- y) (- z))]))

(define (vec-+= v v1)
  (match* (v v1)
    [((vec3 x y z) (vec3 x1 y1 z1))
     (set-vec3-x! v (+ x x1))
     (set-vec3-y! v (+ y y1))
     (set-vec3-z! v (+ z z1))]))
(define (vec-*= v t)
  (match v
    [(vec3 x y z)
     (set-vec3-x! v (* x t))
     (set-vec3-y! v (* y t))
     (set-vec3-z! v (* z t))]))
(define (vec-/= v t)
  (vec-*= v (/ 1 t)))
