; SICP exercise 2.08
;
; Using reasoning analogous to Alyssa's, describe how the difference of two
; intervals may be computed. Define a corresponding subtraction procedure,
; called sub-interval.

; We can be extremely elaborate here, but I don't really want to go there.
; Simply, the minimum is the lower bound of the minuend minus the upper bound
; of the subtrahend and vica-versa.
;
; Here's the code.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))
