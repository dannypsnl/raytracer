#lang racket

(provide (all-defined-out))

(require "vec3.rkt"
         "ray.rkt")

(define (hit? hittable r min max rec)
  (let/cc return
    (match hittable
      [(? list?)
       (define temp-rec (hit-record #f #f #f #f #f))
       (define closest-so-far max)
       (define hit-anything? #f)
       (for ([object hittable])
         (let ([hit-this? (hit? object r min closest-so-far temp-rec)])
           (when hit-this?
             (set! closest-so-far (hit-record-t temp-rec))
             ; rec = temp-rec
             (set-hit-record-p! rec (hit-record-p temp-rec))
             (set-hit-record-normal! rec (hit-record-normal temp-rec))
             (set-hit-record-mat-ptr! rec (hit-record-mat-ptr temp-rec))
             (set-hit-record-t! rec (hit-record-t temp-rec))
             (set-hit-record-front-face! rec (hit-record-front-face temp-rec))
             ; hit-anything?
             (set! hit-anything? #t))))
       hit-anything?]
      [(sphere center radius mat-ptr)
       ; NOTE: convertion
       ; (-b^2 +- √b^2 - 4ac) / 2a
       ; -> (-2h^2 +- √(2h)^2 - 4ac) / 2a
       ; -> (-2h^2 +- 2√h^2 - ac) / 2a
       ; -> (-h^2 +- √h^2 - ac) / a
       (define oc (vec3-- (ray-origin r) center))
       (define a (vec3-length-squared (ray-direction r)))
       (define h (dot oc (ray-direction r)))
       (define c (- (vec3-length-squared oc) (* radius radius)))
       (define discriminant (- (* h h) (* a c)))
       (when (> discriminant 0)
         (let* ([root (sqrt discriminant)]
                [temp (/ (- (+ h root)) a)])
           (when (and (< temp max) (> temp min))
             (set-hit-record-t! rec temp)
             (set-hit-record-p! rec (ray-at r (hit-record-t rec)))
             (set-hit-record-face-normal! rec r (vec3-/ (vec3-- (hit-record-p rec) center) radius))
             (set-hit-record-mat-ptr! rec mat-ptr)
             (return #t))
           (set! temp (/ (- root h) a))
           (when (and (< temp max) (> temp min))
             (begin
               (set-hit-record-t! rec temp)
               (set-hit-record-p! rec (ray-at r (hit-record-t rec)))
               (set-hit-record-face-normal! rec r (vec3-/ (vec3-- (hit-record-p rec) center) radius))
               (set-hit-record-mat-ptr! rec mat-ptr)
               (return #t)))))
       #f])))

(struct sphere (center radius mat-ptr) #:transparent)

(struct hit-record (p normal mat-ptr t front-face)
  #:mutable #:transparent)

(define (set-hit-record-face-normal! rec r outward-normal)
  (define front-face (< (dot (ray-direction r) outward-normal) 0))
  (set-hit-record-normal! rec (if front-face outward-normal (vec3-- outward-normal))))

(define (scatter material r-in rec)
  (match material
    [(lambertian albedo)
     (cons (ray (hit-record-p rec) (vec3-+ (hit-record-normal rec) (random-unit-vec3))) albedo)]
    [(dielectric ref-idx)
     (let* ([etai-over-etat (if (hit-record-front-face rec) (/ 1.0 ref-idx) ref-idx)]
            [unit-direction (unit-vector (ray-direction r-in))]
            [rec.normal (hit-record-normal rec)]
            [cos-theta (min (dot (vec3-- unit-direction) rec.normal) 1.0)]
            [sin-theta (sqrt (- 1.0 (* cos-theta cos-theta)))]
            [reflect-or-refracted (if (> (* etai-over-etat sin-theta) 1.0)
                                      (reflect unit-direction rec.normal)
                                      (refract unit-direction rec.normal etai-over-etat))])
       (cons (ray (hit-record-p rec) reflect-or-refracted) (color 1.0 1.0 1.0)))]
    [(metal albedo fuzz)
     (let* ([reflected (reflect (unit-vector (ray-direction r-in)) (hit-record-normal rec))]
            [scattered (ray (hit-record-p rec) (vec3-+ reflected (vec3-* fuzz (random-in-unit-sphere))))])
       (if (> (dot (ray-direction scattered) (hit-record-normal rec)) 0)
           (cons scattered albedo)
           #f))]))

(struct lambertian (albedo) #:transparent)
(struct dielectric (ref-idx) #:transparent)
(struct metal (albedo fuzz) #:transparent)

(define (mk-metal a f)
  (metal a (if (< f 1) f 1)))
