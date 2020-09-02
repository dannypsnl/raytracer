#lang racket/base

(module+ main
  (define image-width 256)
  (define image-height 256)

  (printf "P3~n ~a ~a~n255~n" image-width image-height)

  (for ([j (in-range (- image-height 1) 0 -1)])
    (eprintf "Scanlines remaining: ~a~n" j)
    (for ([i (in-range 0 image-width)])
      (define r (/ i (- image-width 1)))
      (define g (/ j (- image-height 1)))
      (define b 0.25)

      (define ir (inexact->exact (truncate (* 255.99 r))))
      (define ig (inexact->exact (truncate (* 255.99 g))))
      (define ib (inexact->exact (truncate (* 255.99 b))))

      (printf "~a ~a ~a~n" ir ig ib)))

  (eprintf "~nDone.~n"))
