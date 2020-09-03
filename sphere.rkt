#lang racket

(require "vec3.rkt"
         "ray.rkt"
         "hittable.rkt")

(struct sphere
  (center radius)
  #:methods gen:hittable
  [(define (hit? s r min max rec)
     (let/cc return
       (match-let ([(struct* sphere ([center center] [radius radius])) s])
         (define oc (vec3-- (ray-origin r) center))
         (define a (vec3-length-squared (ray-direction r)))
         (define h (dot oc (ray-direction r)))
         (define c (- (vec3-length-squared oc) (* radius radius)))
         (define discriminant (- (* h h) (* a c)))
         (if (discriminant . > . 0)
             (let* ([root (sqrt discriminant)]
                    [temp (/ (- (- h) root) a)])
               (when (and (temp . < . max) (temp . > . min))
                 (set-hit-record-t! rec temp)
                 (set-hit-record-p! rec (ray-at (hit-record-t rec)))
                 (set-hit-record-face-normal! rec r (/ (- (hit-record-p rec) center) radius))
                 (return #t))
               (set! temp (/ (+ (- h) root) a))
               (when (and (temp . < . max) (temp . > . min))
                 (begin
                   (set-hit-record-t! rec temp)
                   (set-hit-record-p! rec (ray-at (hit-record-t rec)))
                   (set-hit-record-face-normal! rec r (/ (- (hit-record-p rec) center) radius))
                   (return #t))))
             #f))))]
  #:transparent)
