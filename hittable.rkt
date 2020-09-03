#lang racket

(provide (all-defined-out))

(require racket/generic)
(require "vec3.rkt"
         "ray.rkt")

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

(define-generics hittable
  [hit? hittable r min max rec])
