#lang racket

(provide (all-defined-out))

(module+ test
  (require rackunit))
(require racket/performance-hint)
(require "random.rkt")

(struct vec3 (x y z)
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "~a ~a ~a" (vec3-x v) (vec3-y v) (vec3-z v)))]
  #:transparent
  #:mutable)

(define samples-per-pixel 100)
(struct color vec3 ()
  #:methods gen:custom-write
  [(define (write-proc c port mode)
     (define-inline (clamp x min max)
       (cond
         [(< x min) min]
         [(> x max) max]
         [else x]))
     (match-let ([(vec3 r g b) c])
       (let* ([scale (/ 1.0 samples-per-pixel)]
              [r (sqrt (* r scale))]
              [g (sqrt (* g scale))]
              [b (sqrt (* b scale))])
         (fprintf port "~a ~a ~a~n"
                  (inexact->exact (truncate (* 256 (clamp r 0.0 0.999))))
                  (inexact->exact (truncate (* 256 (clamp g 0.0 0.999))))
                  (inexact->exact (truncate (* 256 (clamp b 0.0 0.999))))))))]
  #:transparent)
(struct point3 vec3 () #:transparent)

(define (vec3->color v)
  (color (vec3-x v) (vec3-y v) (vec3-z v)))

(define (vec3-length-squared v)
  (match v
    [(vec3 x y z) (+ (* x x) (* y y) (* z z))]))
(define (vec3-length v)
  (sqrt (vec3-length-squared v)))

(define (vec3-+= v v1)
  (match-let ([(vec3 x y z) v]
              [(vec3 x1 y1 z1) v1])
    (set-vec3-x! v (+ x x1))
    (set-vec3-y! v (+ y y1))
    (set-vec3-z! v (+ z z1))))

(define (vec3-*= v t)
  (match-let ([(vec3 x y z) v])
    (set-vec3-x! v (* x t))
    (set-vec3-y! v (* y t))
    (set-vec3-z! v (* z t))))
(define (vec3-/= v t)
  (vec3-*= v (/ 1 t)))

(define-inline (vec3-+ u v . v*)
  (match-let ([(vec3 x y z) u]
              [(vec3 x1 y1 z1) v])
    (let ([r (vec3 (+ x x1) (+ y y1) (+ z z1))])
      (match v*
        [(? empty?) r]
        [`(,a) (vec3-+ r (car v*))]
        [else (vec3-+ r (car v*) (cdr v*))]))))

(define-inline (vec3-- u . v*)
  (if (empty? v*)
      (match-let ([(vec3 x y z) u])
        (vec3 (- x) (- y) (- z)))
      (foldl (λ (v rv)
               (vec3-+ rv (vec3-- v)))
             u
             v*)))

(define-inline (vec3-* u v)
  (match* (u v)
    [((vec3 x y z) (vec3 x1 y1 z1))
     (vec3 (* x x1) (* y y1) (* z z1))]
    [((vec3 x y z) t) #:when (number? t)
                      (vec3 (* x t) (* y t) (* z t))]
    [(t (vec3 x y z)) #:when (number? t)
                      (vec3-* v t)]))

(define-inline (vec3-/ v t)
  (vec3-* (/ 1 t) v))
(define-inline (dot u v)
  (match-let ([(vec3 x y z) u]
              [(vec3 x1 y1 z1) v])
    (+ (* x x1) (* y y1) (* z z1))))
(define-inline (cross u v)
  (match-let ([(vec3 x y z) u]
              [(vec3 x1 y1 z1) v])
    (vec3 (- (* y z1) (* z y1))
          (- (* z x1) (* x z1))
          (- (* x y1) (* y x1)))))
(define-inline (unit-vector v)
  (vec3-/ v (vec3-length v)))

(define (random-unit-vec3)
  (let* ([a (random-double 0 (* 2 pi))]
         [z (random-double -1 1)]
         [r (sqrt (- 1 (* z z)))])
    (vec3 (* r (cos a))
          (* r (sin a))
          z)))

(define (reflect v n)
  (vec3-- v (vec3-* (* 2 (dot v n)) n)))

(module+ test
  (check-equal? (vec3-+ (vec3 1 2 3) (vec3 1 0 0) (vec3 1 0 0))
                (vec3 3 2 3))
  (check-equal? (vec3-- (vec3 1 2 3) (vec3 1 0 0) (vec3 1 0 0))
                (vec3 -1 2 3))
  (check-equal? (vec3-* (vec3 1 2 3) 2)
                (vec3 2 4 6))
  (check-equal? (vec3-* 2 (vec3 1 2 3))
                (vec3 2 4 6))
  (check-equal? (vec3-* (vec3 1 2 3) (vec3 1 2 3))
                (vec3 1 4 9))
  (check-equal? (vec3-/ (vec3 1 2 3) 2)
                (vec3 1/2 1 3/2)))
