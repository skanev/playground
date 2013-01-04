; EOPL exercise 1.13
;
; In our example, we began by eliminating the Kleene star in the grammar of
; S-list. Write subst following the original grammar by using map.

(define subst
  (lambda (new old slist)
    (map (lambda (sexp) (subst-in-s-exp new old sexp))
         slist)))

(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
      (if (eqv? sexp old) new sexp)
      (subst new old sexp))))
