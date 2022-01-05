#lang typed/racket/base
(provide (struct-out camera)
         mk-camera
         camera-get-ray)

(require "vec3.rkt"
         "ray.rkt")

(struct camera
  ([origin : point3]
   [lower-left-corner : vec3]
   [horizontal : vec3]
   [vertical : vec3])
  #:transparent)

(define (mk-camera) : camera
  (let* ([aspect-ratio (/ 16.0 9.0)]
         [viewport-height 2.0]
         [viewport-width (* aspect-ratio viewport-height)]
         [focal-length 1.0]
         [origin (point3 0. 0. 0.)]
         [horizontal (vec3 viewport-width 0.0 0.0)]
         [vertical (vec3 0.0 viewport-height 0.0)])
    (camera origin
            (vec3-- origin (vec3-/ horizontal 2.) (vec3-/ vertical 2.) (vec3 0. 0. focal-length))
            horizontal
            vertical)))

(define (camera-get-ray [c : camera] [u : Flonum] [v : Flonum])
  (ray (camera-origin c)
       (vec3--
        (vec3-+ (camera-lower-left-corner c)
                (vec3-* u (camera-horizontal c))
                (list (vec3-* v (camera-vertical c))))
        (camera-origin c))))
