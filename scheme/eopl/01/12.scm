; EOPL exercise 1.12
;
; Eliminate the one call to subst-in-s-exp in subst, by replacing it by its
; definition and simplifying the resulting procedure. The result will be a
; version of substs that does not need subst-in-s-exp. This technique is
; called inlining, and is used by optimizing compilers.

(define subst
  (lambda (new old slist)
    (if (null? slist)
      '()
      (cons
        (if (symbol? (car slist))
          (if (eqv? (car slist) old) new (car slist))
          (subst new old (car slist)))
        (subst new old (cdr slist))))))
