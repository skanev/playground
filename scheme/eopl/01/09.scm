; EOPL exercise 1.09
;
; Define remove, which is like remove-first, except that it removes all
; occurences of a given symbol form a list of symbols, not just the first.

; Of course, remove is already defined in Scheme, so I'm going to define
; another function, remove-all, to allow the function body to see itself in
; the enclosing environment.

(define remove-all
  (lambda (s los)
    (cond ((null? los) '())
          ((eq? (car los) s) (remove-all s (cdr los)))
          (#t (cons (car los) (remove-all s (cdr los)))))))

(define remove remove-all)
