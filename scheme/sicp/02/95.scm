; SICP exercise 2.95
;
; ₁₂₃
; Define P₁, P₂, and P₃ to be the polynomials:
;
;     P₁: x² - 2x + 1
;     P₂: 11x² + 7
;     P₃: 13x + 5
;
; Now define Q₁ to be the product of P₁ and P₂ and Q₂ to be the product of P₁
; and P₃, and use greated-common-divisor (exercise 2.94) to compute the GCD of
; Q₁ and Q₂. Note that the answer is not the same as P₁. This example
; introduces noninteger operations into the computation, causing difficulties
; with the GCD algorithms. To understand what is happening, try tracing
; gcd-terms while computing the GCD or try performing the division by hand.

; I'll avoid doing it by hand, since the numbers are not too nice. Here is
; each step from gcd-terms:
;
; +------------------------------------+------------------------------------+
; | quotient                           | remainder                          |
; +------------------------------------+------------------------------------+
; | 11x⁴ + -22x³ + 18x² + -14x + 7     | 13x³ + -21x² + 3x + 5              |
; | 13x³ + -21x² + 3x + 5              | 1458/169x² + -2916/169x + 1458/169 |
; | 1458/169x² + -2916/169x + 1458/169 | 0                                  |
; +------------------------------------+------------------------------------+
;
; As you can see, it goes into divison of non-integers. Since Racket supports
; rational numbers, this is actually a GCD of Q₁ and Q₂, although with
; rational coefficients (as the footnote indicates). It can easily produce
; rounding errors in case of floating-point precision.
