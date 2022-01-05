#lang typed/racket

(provide (all-defined-out))

(require racket/flonum
         "vec3.rkt"
         "ray.rkt")

(define (hit-sphere? [hittable : sphere]
                     [r : ray]
                     [min : Flonum] [max : Flonum]
                     [boxed-rec : (Boxof hit-record)])
  : Boolean
  (match-define (sphere center radius mat-ptr) hittable)
  (define rec (unbox boxed-rec))
  ; NOTE: convertion
  ; (-b^2 +- √b^2 - 4ac) / 2a
  ; -> (-2h^2 +- √(2h)^2 - 4ac) / 2a
  ; -> (-2h^2 +- 2√h^2 - ac) / 2a
  ; -> (-h^2 +- √h^2 - ac) / a
  (let/cc return : Boolean
    (define oc (vec3-- (ray-origin r) center))
    (define a (vec3-length-squared (ray-direction r)))
    (define h (dot oc (ray-direction r)))
    (define c (- (vec3-length-squared oc) (* radius radius)))
    (define discriminant (- (* h h) (* a c)))
    (when (> discriminant 0)
      (let* ([root (sqrt discriminant)]
             [temp (/ (- (+ h root)) a)])
        (when (and (< temp max) (> temp min))
          (set-hit-record-p! rec (ray-at r (hit-record-t rec)))
          (set-hit-record-mat-ptr! rec mat-ptr)
          (set-hit-record-t! rec temp)
          (set-hit-record-face-normal! rec r (vec3-/ (vec3-- (hit-record-p rec) center) radius))
          (set-box! boxed-rec rec)
          (return #t))
        (set! temp (/ (- root h) a))
        (when (and (< temp max) (> temp min))
          (set-hit-record-p! rec (ray-at r (hit-record-t rec)))
          (set-hit-record-mat-ptr! rec mat-ptr)
          (set-hit-record-t! rec temp)
          (set-hit-record-face-normal! rec r (vec3-/ (vec3-- (hit-record-p rec) center) radius))
          (set-box! boxed-rec rec)
          (return #t))))
    #f))
(define (hit? [hittable : (Listof sphere)]
              [r : ray]
              [min : Flonum] [max : Flonum]
              [rec : (Boxof hit-record)])
  : Boolean
  (define boxed-rec (cast (box #f)
                          (Boxof hit-record)))
  (define closest-so-far max)
  (define hit-anything? : Boolean #f)
  (for ([sphere hittable])
    (let ([hit-this? (hit-sphere? sphere r min closest-so-far boxed-rec)])
      (when hit-this?
        (define temp-rec (unbox boxed-rec))
        (set! closest-so-far (hit-record-t temp-rec))
        ; rec = temp-rec
        (set-box! boxed-rec
                  (hit-record (hit-record-p temp-rec)
                              (hit-record-normal temp-rec)
                              (hit-record-mat-ptr temp-rec)
                              (hit-record-t temp-rec)
                              (hit-record-front-face temp-rec)))
        ; hit-anything?
        (set! hit-anything? #t))))
  hit-anything?)

(struct sphere
  ([center : vec3]
   [radius : Flonum]
   [mat-ptr : Any])
  #:transparent)

(struct hit-record
  ([p : vec3]
   [normal : vec3]
   [mat-ptr : Any]
   [t : Flonum]
   [front-face : Any])
  #:mutable #:transparent)

(define (set-hit-record-face-normal! [rec : hit-record] [r : ray] [outward-normal : vec3])
  (define front-face (fl< (dot (ray-direction r) outward-normal) 0.))
  (set-hit-record-normal! rec (if front-face outward-normal (vec3-- outward-normal))))

(define (scatter [material : (U lambertian dielectric metal)] [r-in : ray] [rec : hit-record])
  : (Values (Option ray) (U Flonum color #f))
  (match material
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
       (if (> (dot (ray-direction scattered) (hit-record-normal rec)) 0)
           (values scattered albedo)
           (values #f #f)))]))

(struct lambertian ([albedo : Flonum]) #:transparent)
(struct dielectric ([ref-idx : Flonum]) #:transparent)
(struct metal
  ([albedo : Flonum]
   [fuzz : Flonum])
  #:transparent)

(define (mk-metal [a : Flonum] [f : Flonum])
  (metal a (if (fl< f 1.) f 1.)))
