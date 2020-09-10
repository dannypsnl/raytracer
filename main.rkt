#lang racket

(require "vec3.rkt"
         "ray.rkt"
         "meta.rkt"
         "hittable.rkt"
         "camera.rkt"
         "random.rkt")

(define (ray-color r world depth)
  (define rec (hit-record #f #f #f #f))

  (if (<= depth 0)
      (color 0 0 0)
      (if (and (hit? world r 0.001 +inf.0 rec) (hit-record-normal rec))
          (let* ([rec.p (hit-record-p rec)]
                 [target (vec3-+ rec.p (hit-record-normal rec) (random-in-unit-sphere))])
            (vec3->color (vec3-* 0.5 (ray-color (ray rec.p (vec3-- target rec.p)) world (- depth 1)))))
          (let* ([unit-direction (unit-vector (ray-direction r))]
                 [t (* 0.5 (+ (vec3-y unit-direction) 1.0))])
            (vec3->color (vec3-+ (vec3-* (- 1.0 t) (color 1.0 1.0 1.0))
                                 (vec3-* t (color 0.5 0.7 1.0))))))))

(module+ main
  ; Image
  (define aspect-ratio (/ 16.0 9.0))
  (define image-width 400)
  (define image-height (inexact->exact (truncate (/ image-width aspect-ratio))))
  (define max-depth 50)

  ; World
  (define world (list
                 (sphere (point3 0 0 -1) 0.5)
                 (sphere (point3 0 -100.5 -1) 100)))

  ; Camera
  (define cam (mk-camera))

  ; Render
  (printf "P3~n ~a ~a~n255~n" image-width image-height)

  (for ([j (in-range (- image-height 1) 0 -1)])
    (eprintf "Scanlines remaining: ~a~n" j)
    (for ([i (in-range 0 image-width)])
      (define pixel-color (color 0 0 0))
      (for ([s (in-range 0 samples-per-pixel)])
        (define u (/ (+ i (random-double)) (- image-width 1)))
        (define v (/ (+ j (random-double)) (- image-height 1)))
        (define r (camera-get-ray cam u v))
        (vec3-+= pixel-color (ray-color r world max-depth)))
      (printf "~a~n" pixel-color)))

  (eprintf "~nDone.~n"))
