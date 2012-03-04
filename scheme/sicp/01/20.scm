; SICP exercise 1.20
;
; The process that a procedures generates is of course dependent on the rules
; used by the interpreter. As an example, consider the iterative gcd procedure
; given above. Suppose we were to interpret this procedure using normal-order
; evaluation, as discussed in section 1.1.5. (The normal-order-evaluation rule
; for if is described in exercise 1.5.) Using the substitution method (for
; normal order), illustrate the process generated in evaluating (gcd 206 40)
; and indicate the remainder operations that are actually performed. How many
; remainder operations are actually performed in the normal-order evaluation of
; (gcd 206 40)? In the applicative-order evaluation?

; The procedure is:
;
; (define (gcd a b)
;   (if (= b 0)
;       a
;       (gcd b (remainder a b))))
;
; I am going to abbreviate (remainder a b) to (r a b) to keep things managable.
;
; Using normal-order evaluation, we take the following steps:
;
; (gcd 206 40)
; (if (= 40 0) 206 (gcd 40 (r 206 40)))
; (gcd 40 (r 206 40))
; (if (= (r 206 40) 0) 40 (gcd (r 206 40) (r 40 (r 206 40))))
; (if (= 6 0) 40 (gcd (r 206 40) (r 40 (r 206 40))))
; (gcd (r 206 40) (r 40 (r 206 40)))
; (if (= (r 40 (r 206 40)) 0) (r 206 40) (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
; (if (= (r 40 6) 0) (r 206 40) (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
; (if (= 4 0) (r 206 40) (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
; (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
; (if (= (r (r 206 40) (r 40 (r 206 40))) 0) (r 40 (r 206 40)) (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
; ...
;
; If we just take a look at each iteration, we see:
;
; (gcd 206 40)
; (gcd 40 (r 206 40))
; (gcd (r 206 40) (r 40 (r 206 40)))
; (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
; (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
; (r (r 206 40) (r 40 (r 206 40)))
;
; On each step we calculate b and then we calculate the final a. Thus, we use
; 18 remainder operations.
;
; Using applicative order-evaluation, we take the following steps
;
; (gcd 206 40)
; (gcd 40 (r 206 40))
; (gcd 6 (r 40 6))
; (gcd 4 (r 6 4))
; (gcd 2 (r 4 2))
; 2
;
; We end up using 4 remainder operations.
