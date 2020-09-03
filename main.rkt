#lang racket/base

(require "vec3.rkt")

(module+ main
  (define image-width 256)
  (define image-height 256)

  (printf "P3~n ~a ~a~n255~n" image-width image-height)

  (for ([j (in-range (- image-height 1) 0 -1)])
    (eprintf "Scanlines remaining: ~a~n" j)
    (for ([i (in-range 0 image-width)])
      (define pixel-color
        (color (/ i (- image-width 1))
               (/ j (- image-height 1))
               0.25))
      (printf "~a" pixel-color)))

  (eprintf "~nDone.~n"))
