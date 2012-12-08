; SICP exercise 4.17
;
; Draw diagrams of the environment in effect when evaluating the expression
; <e3> in the procedure in the text, comparing how this will be structured when
; definitions are interpreted sequentially with how it will be structured if
; definitions are scanned out as described. Why is there an extra frame in the
; transformed program? Explain why this difference in environment structure can
; never make a difference in the behavior of a correct program. Design a way to
; make the interpreter implement the "simultaneous" scope rule for internal
; definitions without constructing the extra frame.

; Here's the diagram of the sequential interpretation:
;
; <lambda>: +----------------------+
;           | <vars>: ...          |
;           +----------------------+
;                      ^
;                      |
;    <let>: +----------------------+
;           | u: <e1>              |
;           | v: <e2>              |
; <e3>----> +----------------------+
;
; In comparison, this is the environment with the sequential interpretation:
;
; <lambda>: +----------------------+
;           | <vars>: ...          |
;           | u: <e1>              |
;           | v: <e2>              |
; <e3>----> +----------------------+
;
; The extra frame is there because of the let statement, which is implemented
; as a lambda invocation. It does not make a difference, because the variables
; have the same values in both environments. set! will affect different
; environments in the two cases, but the value in the inner environment will
; still be the same. Since this construction does not allow us to create
; outside of the let, we cannot create a new function that will use the
; environment of the lambda, but not the environment of the let.
;
; If we want to remove the extra frame, we can do it just by rearranging the
; body of the procedure and moving all the definitions to the top, kind of like
; function hoisting in JavaScript. In that case we will only have one
; environemnt.
