; SICP exercise 1.29
;
; Simpson's Rule is a more accurate method of numerical integration than the
; method illustrated above. Using Simpson's Rule, the integral of a function f
; between a and b is approximated as
;
; h
; ─(y₀ + 4y₁ + 2y₂ + 4y₃ + 2y₄ + ... + 2yₙ₋₂ + 4yₙ₋₁ + yₙ)
; 3
;
; where h = (b - a)/n, for some even integer n, and yₖ = f(a + kh). (Increasing
; n increases the accuracy of the approximation.) Define a procedure that takes
; as arguments f, a, b and n and returns the value of the integral, computed
; using Simpson's Rule. Use your procedure to integrate cube between 0 and 1
; (with n = 100 and n = 1000), and compare the results to those of the integral
; procedure shown above.

; Implementing the integral with Simpson's Rule is easy. Check it out below.
; Comparing the results to integral, however, appears way harder. The results
; do not match my expectation, at least. Here's what I get from integral:
;
; +----------------+---------------------+---------------------+
; | Iterations     | integral            | simpson-integral    |
; +----------------+---------------------+---------------------+
; |   100   (0.01) | 0.24998750000000042 | 0.24671666666666678 |
; |  1000  (0.001) | 0.24999987500000100 | 0.24966716666666610 |
; | 10000 (0.0001) | 0.24999999874993412 | 0.24996667166666647 |
; +----------------+---------------------+---------------------+
;
; I have no idea why it doesn't match my expectation.

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (next k)
    (+ 1 k))
  (define (coefficient k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((odd? k) 2)
          (else 4)))
  (define (term k)
    (* (coefficient k)
       (f (+ a (* k h)))))
  (* (/ h 3)
     (sum term 0 next n)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (cube x)
  (* x x x))
