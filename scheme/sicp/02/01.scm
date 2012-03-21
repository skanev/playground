; SICP exercise 2.01
;
; Define a better version of make-rat that handles both positive and negative
; arguments. make-rat should normalize the sign so that if the rational number
; is positive, both the numerator and denominator are positive, and if the
; rational number is negative, only the numerator is negative.

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g))
          (d (/ d g)))
      (if (< d 0)
          (cons (- n) (- d))
          (cons n d)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))
