; SICP exercise 4.15
;
; Given a one-argument procedure p and an object a, p is said to "halt" on a if
; evaluating the expresssion (p a) returns a value (as opposed to terminating
; with an error message or running forever). Show that it is impossible to
; write a procedure halts? that correctly determines whether p halts on a for
; any procedure p and object a. Use the following reasoning: If you had such a
; procedure halts?, you could implement the following program:
;
;   (define (run-forever) (run-forever))
;
;   (define (try p)
;     (if (halts? p p)
;         (run-forever)
;         'halted))
;
; Now consider evaluating the expression (try try) and show that any possible
; outcome (either halting or running) violates the intended behavior of halts?

; try checks if the argument will halt when given itself. In case it halts, try
; will run forever and halt otherwise. That is, try has the inverse behavior of
; the passed procedure. So what happens when we pass try to itself?
;
; Let's assume that (halts? try try) returns true. In that case, (try try) will
; run forever, which is a violation of our assumption. We're led to believe
; that (halts? try try) should return false. But in that case, if we run (try
; try), it will halt and return 'halted, which again, is a violation of our
; assumption.
;
; We have to accept that halts? cannot exist.
