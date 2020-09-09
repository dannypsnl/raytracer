#lang racket

(provide (all-defined-out))

(require "vec3.rkt"
         "ray.rkt")

(struct/contract camera
                 ([origin point3?]
                  [lower-left-corner point3?]
                  [horizontal vec3?]
                  [vertical vec3?])
                 #:transparent)

(define (mk-camera)
  (let* ([aspect-ratio (/ 16.0 9.0)]
         [viewport-height 2.0]
         [viewport-width (* aspect-ratio viewport-height)]
         [focal-length 1.0]
         [origin (point3 0 0 0)]
         [horizontal (vec3 viewport-width 0.0 0.0)]
         [vertical (vec3 0.0 viewport-height 0.0)])
    (camera origin
            (vec3-- origin (/ horizontal 2) (/ vertical 2)
                    (vec3 0 0 focal-length))
            horizontal
            vertical)))

(define/contract (camera-get-ray c u v)
  (-> camera? number? number? ray?)
  (ray (camera-origin c)
       (vec3--
        (vec3-+ (camera-lower-left-corner c)
                (camera-horizontal c)
                (camera-vertical c))
        (camera-origin c))))
