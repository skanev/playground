; EOPL exercise 1.11
;
; In the last line of subts-in-s-exp, the recursion is on sexp and not a
; smaller substructure. Why is the recursion guaranteed to halt?

; The recursion might be on sexp, but it calls subst instead. The substs
; procedure will reduce the problem to a smaller problem, which would
; guarantee that it will halt.
