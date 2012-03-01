; SICP exercise 1.05
;
; Ben Bitdiddle has invented a test to determine whether the interpreter he is
; faced with is using applicative-order evaluation or normal-order evaluation.
; He defines the following two procedures:
;
; (define (p) (p))
;
; (define (test x y)
;   (if (= x 0)
;       0
;       y))
;
; Then he evaluates he expression
;
; (test 0 (p))
;
; What behavior will Ben observe with an interpreter that uses
; applicative-order evaluation? What behavior will he observe with an
; interpreter that users normal-order evaluation? Explain your answer. (Assume
; that the evaluation rule of the special form if is the same whether the
; interpreter is using normal or applicative order: The predicate expression is
; evaluated first, and the result determines whether to evaluate the consequent
; or the alternative expression.)

; p does not depend on the type of evaluation, while test does.
;
; (p) is an infinite loop - the interpreter will never stop evaluating it,
; since on every evaluation it has to evaluate it again.
;
; If the first argument of x equals 0, it does not use the second. Within
; normal-order evaluation, the second argument will never be evaluated. Within
; applicative-order evaluation, the second argument gets evaluated before test
; is evaluated. Thus, we can tell the type of evaluation by observing if the
; second argument gets evaluated.
;
; Within applicative-order evaluation the second argument will be evaluated
; before test and the interpreter will get stuck in an infinite loop. Within
; normal-order evaluation the second argument will never get evaluated and the
; interpreter will finish successfully. We can use that to tell which
; evaluation type we are using.
