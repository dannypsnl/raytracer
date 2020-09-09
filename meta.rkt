#lang racket

(provide degrees-to-radians)

(require racket/performance-hint)

(define-inline (degrees-to-radians degrees)
  ((* degrees pi) . / . 180.0))
