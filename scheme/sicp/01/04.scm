; SICP exercise 1.04
;
; Observe that our model of evaluation allows for combinations whose operators
; are compound expressions. Use this observation to describe the behavior of
; the following procedure
;
; (define (a-plus-abs-b a b)
;   ((if (> b 0) + -) a b))

; The procedure adds a to the absolute value of b, pretty much as it name
; suggests. Instead of calculating the absolute value of b and adding it
; directly to a, we use the fact that:
;
;   a + |b| = { a + b     if b is positive
;             { a - b     if b is negative
;
; In both cases we have an expression with similar structure, but a different
; operator: (<operator> a b). We can determine which operator to use by
; comparing b to 0, which is what (if (> b 0) + -) does. In the former case
; it evaluates to +, while in the later it evaluates to -.
