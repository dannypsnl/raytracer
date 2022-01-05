#lang typed/racket
(provide (struct-out ray)
         ray-at)

(require "vec3.rkt")

(struct ray
  ([origin : vec3]
   [direction : vec3])
  #:transparent)

(: ray-at : ray Float -> vec3)
(define (ray-at r t)
  (match r
    [(ray origin direction)
     (vec3-+ origin
             (vec3-* t direction))]))
