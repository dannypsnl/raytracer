#lang racket/base

(require "vec3.rkt"
         "ray.rkt")

(define (ray-color r)
  (define unit-direction (unit-vector (ray-direction r)))
  (define t (* 0.5 (+ (vec3-y unit-direction) 1.0)))
  (vec3-+ (vec3-* (- 1.0 t) (color 1.0 1.0 1.0))
          (vec3-* t (color 0.5 0.7 1.0))))

(module+ main
  ; Image
  (define aspect-ratio (/ 16.0 9.0))
  (define image-width 400)
  (define image-height (inexact->exact (truncate (/ image-width aspect-ratio))))

  ; Camera
  (define viewport-height 2.0)
  (define viewport-width (* aspect-ratio viewport-height))
  (define focal-length 1.0)

  (define origin (point3 0 0 0))
  (define horizontal (vec3 viewport-width 0 0))
  (define vertical (vec3 0 viewport-height 0))
  (define lower-left-corner
    (vec3-- (vec3-- (vec3-- origin (vec3-/ horizontal 2)) (vec3-/ vertical 2)) (vec3 0 0 focal-length)))

  (printf "P3~n ~a ~a~n255~n" image-width image-height)

  (for ([j (in-range (- image-height 1) 0 -1)])
    (eprintf "Scanlines remaining: ~a~n" j)
    (for ([i (in-range 0 image-width)])
      (define u (/ i (- image-width 1)))
      (define v (/ j (- image-height 1)))
      (define r (ray origin (vec3-- (vec3-+ lower-left-corner
                                            (vec3-+ (vec3-* u horizontal)
                                                    (vec3-* v vertical)))
                                    origin)))
      (define pixel-color (ray-color r))
      (printf "~a" pixel-color)))

  (eprintf "~nDone.~n"))
