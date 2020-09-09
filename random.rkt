#lang racket

(require racket/performance-hint)

(define-inline (random-double [min 0] [max 1])
  (+ min (* (- max min) (random))))
