#lang racket

(provide (all-defined-out))

(require racket/generic)
(require "vec3.rkt"
         "ray.rkt")

(define-generics hittable
  (hit? hittable r min max rec)
  #:defaults ([list?
               (define (hit? hittable* r min max rec)
                 (define temp-rec (hit-record #f #f #f #f))
                 (define closest-so-far max)
                 (ormap (λ (object)
                          (let ([hit-this? (hit? object r min closest-so-far temp-rec)])
                            (when hit-this?
                              (set! closest-so-far (hit-record-t temp-rec))
                              (set-hit-record-p! rec (hit-record-p temp-rec))
                              (set-hit-record-normal! rec (hit-record-normal temp-rec))
                              (set-hit-record-t! rec (hit-record-t temp-rec))
                              (set-hit-record-front-face! rec (hit-record-front-face temp-rec)))
                            hit-this?))
                        hittable*))]))

(struct/contract hit-record ([p point3?]
                             [normal vec3?]
                             [t number?]
                             [front-face boolean?])
                 #:mutable #:transparent)

(define (set-hit-record-face-normal! rec r outward-normal)
  (define front-face ((dot (ray-direction r) outward-normal) . < . 0))
  (set-hit-record-normal!
   (if front-face
       outward-normal
       (vec3-neg outward-normal))))
