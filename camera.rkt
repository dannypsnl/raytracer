#lang racket

(provide (all-defined-out))

(require racket/performance-hint)
(require "vec3.rkt"
         "ray.rkt")

(struct camera
  (origin
   lower-left-corner
   horizontal
   vertical) #:transparent)

(define-inline (mk-camera)
  (let* ([aspect-ratio (/ 16.0 9.0)]
         [viewport-height 2.0]
         [viewport-width (* aspect-ratio viewport-height)]
         [focal-length 1.0]
         [origin (point3 0 0 0)]
         [horizontal (vec3 viewport-width 0.0 0.0)]
         [vertical (vec3 0.0 viewport-height 0.0)])
    (camera origin
            (vec3-- origin (vec3-/ horizontal 2) (vec3-/ vertical 2) (vec3 0 0 focal-length))
            horizontal
            vertical)))

(define-inline (camera-get-ray c u v)
  (ray (camera-origin c)
       (vec3--
        (vec3-+ (camera-lower-left-corner c)
                (vec3-* u (camera-horizontal c))
                (vec3-* v (camera-vertical c)))
        (camera-origin c))))
