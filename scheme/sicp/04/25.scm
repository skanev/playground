; SICP exercise 4.25
;
; Suppose that (in ordinary applicative-order Scheme) we define unless as
; shown above and then define factorial in terms of unless as
;
;   (define (factorial n)
;     (unless (= n 1)
;             (* n (factorial (- n 1)))
;             1))
;
; What happens if we attempt to evaluate (factorial 5)? Will our definitions
; work in a normal-order language?

; The interpreter will end up in a bottomless recursion, since invoking
; factorial always invokes factorial before returning (even before calling
; unless). Simple, it would not do.
;
; In a normal-order language this definition will work, since the arguments to
; unless won't be evaluated unless they are needed.
