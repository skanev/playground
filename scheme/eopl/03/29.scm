; EOPL exercise 3.29
;
; Unfortunatelly, programs that use dynamic binding may be exceptionally
; difficult to understand. For example, undex lexical binding, consistently
; renaming the bound variables of a procedure can never change the behavior of
; a program: we can even remove all variables and replace them by their
; lexical addresses, as in section 3.6. But under dynamic binding, this
; transformation is unsafe.
;
; For example, under dynamic binding, the procedure proc (z) a returns the
; value of the variable a in the caller's environment. Thus, the program
;
;   let a = 3
;   in let p = proc (z) a
;      in let f = proc (x) (p 0)
;         in let a = 5
;            in (f 2)
;
; returns 5, since a's value at the call site is 5. What if f's formal
; parameter were a?

; It would return 2. The test demonstrates it.

(load-relative "28.scm")

(define the-hypothetical-program
  "let a = 3
   in let p = proc (z) a
      in let f = proc (a) (p 0)
         in let a = 5
            in (f 2)")
