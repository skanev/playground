; SICP exercise 2.16
;
; Explain, in general, why equivalent algebraic expressions may lead to
; different answers. Can you devise an interval-arithmetic package that does
; not have this shortcoming, or is this task impossible? (Warning: This problem
; is very difficult.)

; After doing exercise 2.14, it is very easy to see that multiplying and
; dividing uncertain quantities increases the uncertainty. AA/A is equivalent
; to A, but the error margin is three times bigger. The less we use an
; uncertain quantity in an operation, the smaller tolerance we get.
;
; As for addressing the shortcoming, the only way I can figure out is to
; have the program simplify the expression as much as possible before
; calculating it. I assume that this requires some serious mathematical
; and computer science foundations, that I currently lack.
;
; One idea is to calculate this the expressions lazily. We can collect a tree
; that represents the expression, until the result needs to be calculated. At
; that point, we can simplify the expression and return the result. This won't
; reduce the error margin, but may provide a nicer API.
