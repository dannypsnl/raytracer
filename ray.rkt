#lang typed/racket

(provide (struct-out ray)
         ray-at)

(require/typed "vec3.rkt"
               [#:struct vec3 ([x : Flonum]
                               [y : Flonum]
                               [z : Flonum])]
               [vec3-+ (-> vec3 vec3 vec3)]
               [vec3-* (-> (U vec3 Flonum) (U vec3 Flonum) vec3)])

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
