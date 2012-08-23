; SICP exercise 2.75
;
; Implement the constructor make-from-mag-ang in message-passing style. This
; procedure should be analogousto the make-from-real-imag procedure given
; above.

(define (apply-generic op arg) (arg op))

(define (magnitute imag) (apply-generic 'magnitute imag))
(define (angle imag) (apply-generic 'angle imag))
(define (real-part imag) (apply-generic 'real-part imag))
(define (imag-part imag) (apply-generic 'imag-part imag))

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'magnitute) mag)
          ((eq? op 'angle) ang)
          ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          (else (error "Unknown op - MAKE-FROM-MAG-ANG" op))))
  dispatch)

