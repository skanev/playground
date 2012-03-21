; SICP exercise 1.39
;
; A continued fraction representation of tangent function was published in
; 1770 by the German mathematician J. H. Lambert:
;
;                x
; tan(x) = ─────────────
;                  x²
;          1 - ─────────
;                    x²
;              3 - ─────
;                  5 - ⋱
;
; where x is in radians. Define a procedure (tan-cf x k) that computes an
; approximation to the tangent function based on Lambert's formula. x specifies
; the number of terms to compute, as in exercise 1.37.

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (tan-cf x k)
  (let ((neg-x-squared (- (* x x))))
    (cont-frac (lambda (n) (if (= n 1) x neg-x-squared))
               (lambda (n) (- (* n 2) 1))
               k)))
