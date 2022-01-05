#lang typed/racket
(provide (all-defined-out))

(require racket/flonum
         "random.rkt")

(struct vec3
  ([x : Flonum]
   [y : Flonum]
   [z : Flonum])
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "~a ~a ~a" (vec3-x v) (vec3-y v) (vec3-z v)))
  #:transparent
  #:mutable)

(define samples-per-pixel 100)
(struct color vec3 ()
  #:property prop:custom-write
  (λ (c port mode)
    (define (clamp [x : Flonum] [min : Flonum] [max : Flonum])
      (cond
        [(< x min) min]
        [(> x max) max]
        [else x]))
    (match-let ([(vec3 r g b) c])
      (let* ([scale (/ 1.0 samples-per-pixel)]
             [r (flsqrt (fl* r scale))]
             [g (flsqrt (fl* g scale))]
             [b (flsqrt (fl* b scale))])
        (fprintf port "~a ~a ~a~n"
                 (inexact->exact (truncate (* 256 (clamp r 0.0 0.999))))
                 (inexact->exact (truncate (* 256 (clamp g 0.0 0.999))))
                 (inexact->exact (truncate (* 256 (clamp b 0.0 0.999))))))))
  #:transparent)
(struct point3 vec3 () #:transparent)

(define (vec3->color [v : vec3]) : color
  (color (vec3-x v) (vec3-y v) (vec3-z v)))

(define (vec3-length-squared [v : vec3]) : Flonum
  (match v
    [(vec3 x y z) (fl+ (fl+ (fl* x x) (fl* y y))
                       (fl* z z))]))
(define (vec3-length [v : vec3]) : Flonum
  (flsqrt (vec3-length-squared v)))

(define (vec3-+= [v : vec3] [v1 : vec3])
  (match-let ([(vec3 x y z) v]
              [(vec3 x1 y1 z1) v1])
    (set-vec3-x! v (+ x x1))
    (set-vec3-y! v (+ y y1))
    (set-vec3-z! v (+ z z1))))

(define (vec3-*= [v : vec3] [t : Flonum])
  (match-let ([(vec3 x y z) v])
    (set-vec3-x! v (fl* x t))
    (set-vec3-y! v (fl* y t))
    (set-vec3-z! v (fl* z t))))
(define (vec3-/= [v : vec3] [t : Flonum])
  (vec3-*= v (fl/ 1. t)))

(: vec3-+ : (case->
             [vec3 vec3 -> vec3]
             [vec3 vec3 (Listof vec3) -> vec3]))
(define vec3-+
  (case-lambda
    [([u : vec3] [v : vec3])
     (match-let ([(vec3 x y z) u]
                 [(vec3 x1 y1 z1) v])
       (vec3 (+ x x1) (+ y y1) (+ z z1)))]
    [([u : vec3] [v : vec3] [v* : (Listof vec3)])
     (match-let ([(vec3 x y z) u]
                 [(vec3 x1 y1 z1) v])
       (let ([r (vec3 (+ x x1) (+ y y1) (+ z z1))])
         (match v*
           [(? empty?) r]
           [`(,a) (vec3-+ r (car v*))]
           [else (vec3-+ r (car v*) (cdr v*))])))]))

(define (vec3-- [u : vec3] v* : vec3 *) : vec3
  (if (empty? v*)
      (match-let ([(vec3 x y z) u])
        (vec3 (- x) (- y) (- z)))
      (foldl (λ ([v : vec3] [rv : vec3])
               (vec3-+ rv (vec3-- v)))
             u
             v*)))

(define (vec3-* [u : (U vec3 Flonum)] [v : (U vec3 Flonum)]) : vec3
  (match* (u v)
    [((vec3 x y z) (vec3 x1 y1 z1))
     (vec3 (* x x1) (* y y1) (* z z1))]
    [((vec3 x y z) t) #:when (flonum? t)
                      (vec3 (* x t) (* y t) (* z t))]
    [(t (vec3 x y z)) #:when (flonum? t)
                      (vec3-* v t)]))

(define (vec3-/ [v : vec3] [t : Flonum])
  (vec3-* (fl/ 1. t) v))
(define (dot [u : vec3] [v vec3]) : Flonum
  (match-let ([(vec3 x y z) u]
              [(vec3 x1 y1 z1) v])
    (fl+ (fl+ (fl* x x1) (fl* y y1))
         (fl* z z1))))
(define (cross [u : vec3] [v : vec3])
  (match-let ([(vec3 x y z) u]
              [(vec3 x1 y1 z1) v])
    (vec3 (- (* y z1) (* z y1))
          (- (* z x1) (* x z1))
          (- (* x y1) (* y x1)))))
(define (unit-vector [v : vec3])
  (vec3-/ v (vec3-length v)))

(define (random-unit-vec3) : vec3
  (let* ([a (random-double 0. (fl* 2. pi))]
         [z (random-double -1. 1.)]
         [r (flsqrt (fl- 1. (fl* z z)))])
    (vec3 (fl* r (flcos a))
          (fl* r (flsin a))
          z)))

(define (random-in-unit-sphere) : vec3
  (define (vec3-random [min : Flonum] [max : Flonum])
    (vec3 (random-double min max) (random-double min max) (random-double min max)))
  (define p (vec3-random -1. 1.))
  (if (>= (vec3-length-squared p) 1)
      (random-in-unit-sphere)
      p))

(define (reflect [v : vec3] [n : vec3]) : vec3
  (vec3-- v (vec3-* (* 2 (dot v n)) n)))

(define (refract [uv : vec3] [n : vec3] [etai-over-etat : Flonum]) : vec3
  (define cos-theta (min (dot (vec3-- uv) n) 1.0))
  (define r-out-perp (vec3-* etai-over-etat (vec3-+ uv (vec3-* cos-theta n))))
  (define r-out-parallel (vec3-* (- (sqrt (abs (- 1.0 (vec3-length-squared r-out-perp))))) n))
  (vec3-+ r-out-perp r-out-parallel))

(module+ test
  (require typed/rackunit)

  (check-equal? (vec3-+ (vec3 1. 2. 3.) (vec3 1. 0. 0.) (list (vec3 1. 0. 0.)))
                (vec3 3. 2. 3.))
  (check-equal? (vec3-- (vec3 1. 2. 3.) (vec3 1. 0. 0.) (vec3 1. 0. 0.))
                (vec3 -1. 2. 3.))
  (check-equal? (vec3-* (vec3 1. 2. 3.) 2.)
                (vec3 2. 4. 6.))
  (check-equal? (vec3-* 2. (vec3 1. 2. 3.))
                (vec3 2. 4. 6.))
  (check-equal? (vec3-* (vec3 1. 2. 3.) (vec3 1. 2. 3.))
                (vec3 1. 4. 9.))
  (check-equal? (vec3-/ (vec3 1. 2. 3.) 2.)
                (vec3 0.5 1. 1.5)))
