#lang typed/racket/base
(provide random-double)

(require racket/flonum)

(define (random-double [min : Flonum 0.] [max : Flonum 1.]) : Flonum
  (fl+ min (fl* (fl- max min) (flrandom (current-pseudo-random-generator)))))
