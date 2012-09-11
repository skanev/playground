; SICP exercise 3.77
;
; The integral procedure used above was analogous to the "implicit" definition
; of the infinite stream of integers in section 3.5.2. Alternatively, we can
; give a definition of integral that is more like integres-strating-from (also
; in section 3.5.2):
;
;   (define (integral integrand initial-value dt)
;     (cons-stream initial-value
;                  (if (stream-null? integrand)
;                      the-empty-stream
;                      (integral (stream-cdr integrand)
;                                (+ (* dt (stream-car integrand))
;                                   initial-value)
;                                dt))))
;
; When used in systems with loops, this procedure has the same problem as does
; our original version of integral. Modify the procedure so that it expects
; the integrand as a delayed argument and hence can be used in the solve
; procedure shown above.

; This is very hard to test, given how Racket works. Anyway:

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? delayed-integrand)
                   the-empty-stream
                   (let ((integrand (force delayed-integrand)))
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (force (stream-car integrand)))
                                  initial-value)
                               dt)))))
