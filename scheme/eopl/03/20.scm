; EOPL exercise 3.20
;
; In PROC, procedures have only one argument, but one can get the effect of
; multiple argument procedures by using procedures that return other
; procedures. For example, one might write code like
;
;     let f = proc (x) proc (y) ...
;     in ((f 3) 4)
;
; The trick is called currying, and the procedure is said to be curried. Write
; a curried procedure that takes two arguments and returns their sum. You can
; write x + y in our language by writing -(x, -(0, y)).

(load-relative "cases/proc/all.scm")

(define two-plus-three
  "let add = proc (x) proc (y) -(x, -(0, y))
       in ((add 2) 3)")
