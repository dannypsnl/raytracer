#lang racket

(provide (all-defined-out))

(require "vec3.rkt")

(struct ray (origin direction) #:transparent)

(define (ray-at r t)
  (match r
    [(ray origin direction)
     (vec3-+ origin
             (vec3-* t direction))]))
