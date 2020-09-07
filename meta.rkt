#lang racket

(provide degrees-to-radians)

(define (degrees-to-radians degrees)
  ((* degrees pi) . / . 180.0))
