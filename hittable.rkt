#lang typed/racket/base

(provide (all-defined-out))

(require racket/flonum
         racket/match
         "vec3.rkt"
         "ray.rkt")

(define-type hittable (U sphere))
(struct sphere
  ([center : vec3]
   [radius : Flonum]
   [mat-ptr : material])
  #:transparent)

(define (set-hit-record-face-normal! [rec : hit-record] [r : ray] [outward-normal : vec3])
  (define front-face (fl< (dot (ray-direction r) outward-normal) 0.))
  (set-hit-record-front-face! rec front-face)
  (set-hit-record-normal! rec (if front-face outward-normal (vec3-- outward-normal))))

(define (hit-object? [object : hittable]
                     [r : ray]
                     [min : Flonum] [max : Flonum]
                     [rec : hit-record])
  : Boolean
  (match-define (sphere center radius mat-ptr) object)
  ; NOTE: convertion
  ; (-b^2 +- √b^2 - 4ac) / 2a
  ; -> (-2h^2 +- √(2h)^2 - 4ac) / 2a
  ; -> (-2h^2 +- 2√h^2 - ac) / 2a
  ; -> (-h^2 +- √h^2 - ac) / a
  (define oc (vec3-- (ray-origin r) center))
  (define a (vec3-length-squared (ray-direction r)))
  (define h (dot oc (ray-direction r)))
  (define c (fl- (vec3-length-squared oc) (fl* radius radius)))
  (define discriminant (fl- (fl* h h) (fl* a c)))
  (if (fl> discriminant 0.)
      (let* ([root (flsqrt discriminant)]
             [temp (fl/ (fl- 0. (fl+ h root)) a)])
        (cond
          [(and (fl< temp max) (fl> temp min))
           (set-hit-record-p! rec (ray-at r (hit-record-t rec)))
           (set-hit-record-mat-ptr! rec mat-ptr)
           (set-hit-record-t! rec temp)
           (set-hit-record-face-normal! rec r (vec3-/ (vec3-- (hit-record-p rec) center) radius))
           #t]
          [else (set! temp (fl/ (fl- root h) a))
                (cond
                  [(and (fl< temp max) (fl> temp min))
                   (set-hit-record-p! rec (ray-at r (hit-record-t rec)))
                   (set-hit-record-mat-ptr! rec mat-ptr)
                   (set-hit-record-t! rec temp)
                   (set-hit-record-face-normal! rec r (vec3-/ (vec3-- (hit-record-p rec) center) radius))
                   #t]
                  [else #f])]))
      #f))
(define (hit? [objects : (Listof hittable)]
              [r : ray]
              [min : Flonum]
              [max : Flonum])
  : (Values Boolean hit-record)
  (define closest-so-far max)
  (define rec (hit-record (vec3 0. 0. 0.)
                          (vec3 0. 0. 0.)
                          (lambertian (color 0.1 0.2 0.5))
                          0.
                          #f))
  (define hit-anything? : Boolean #f)
  (for ([object objects])
    (let ([hit-this? (hit-object? object r min closest-so-far rec)])
      (when hit-this?
        (set! closest-so-far (hit-record-t rec))
        ; hit-anything?
        (set! hit-anything? #t))))
  (values hit-anything? rec))

(struct hit-record
  ([p : vec3]
   [normal : vec3]
   [mat-ptr : material]
   [t : Flonum]
   [front-face : Boolean])
  #:mutable #:transparent)

(define (scatter [r-in : ray] [rec : hit-record])
  : (Values (Option ray) (U Flonum color #f))
  (match (hit-record-mat-ptr rec)
    [(lambertian albedo)
     (values (ray (hit-record-p rec) (vec3-+ (hit-record-normal rec) (random-unit-vec3))) albedo)]
    [(dielectric ir)
     (let* ([refraction-ratio (if (hit-record-front-face rec) (fl/ 1.0 ir) ir)]
            [unit-direction (unit-vector (ray-direction r-in))]
            [rec.normal (hit-record-normal rec)]
            [cos-theta (flmin (dot (vec3-- unit-direction) rec.normal) 1.0)]
            [sin-theta (flsqrt (fl- 1.0 (fl* cos-theta cos-theta)))]
            [direction (if (fl> (fl* refraction-ratio sin-theta) 1.0)
                           (reflect unit-direction rec.normal)
                           (refract unit-direction rec.normal refraction-ratio))])
       (values (ray (hit-record-p rec) direction) (color 1.0 1.0 1.0)))]
    [(metal albedo fuzz)
     (let* ([reflected (reflect (unit-vector (ray-direction r-in)) (hit-record-normal rec))]
            [scattered (ray (hit-record-p rec) (vec3-+ reflected (vec3-* fuzz (random-in-unit-sphere))))])
       (if (fl> (dot (ray-direction scattered) (hit-record-normal rec)) 0.)
           (values scattered albedo)
           (values #f #f)))]))

(define-type material (U lambertian dielectric metal))
(struct lambertian ([albedo : color]) #:transparent)
(struct dielectric ([ref-idx : Flonum]) #:transparent)
(struct metal
  ([albedo : color]
   [fuzz : Flonum])
  #:transparent)

(define (mk-metal [a : color] [f : Flonum])
  (metal a (if (fl< f 1.) f 1.)))
