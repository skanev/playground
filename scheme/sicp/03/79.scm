; SICP exercise 3.79
;
; Generlize the solve-2nd procedure of exercise 3.78 so that it can be used to
; solve general second-order differential equations d²y/dt² = f(dy/dt, y).

(define (solve-2nd-generic f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)
