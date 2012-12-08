; SICP exercise 4.28
;
; Eval uses actual-value rather than eval to evaluate the operator before
; passing it to apply, in order to force the value of the operator. Give an
; example that demonstrates the need for this forcing.

; An example can be the following (rather weird) code:
;
;   ((lambda (f) (f 2))
;    (lambda (x) (* x 2)))
;
; In the working interpreter it should return 4, but if we use evaluate
; instead of actual-value, it will result to an error. The reason is that
; apply would receive a thunk instead of a procedure and the cond will fail to
; match in either clauses (because a thunk is not a primitive-procedure? and
; is not a compound-procedure?).
