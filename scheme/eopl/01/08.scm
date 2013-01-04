; EOPL exercise 1.08
;
; In the definition of remove-first, if the last line were replaced by
; (remove-first s (cdr los)), what function would the resulting procedure
; compute? Give the contract, including the usage statement, for the revised
; procedure.

; drop-until: Sym ╳ Listof(Sym) → Listof(Sym)
; usage: (drop-until s los) returns the suffix of the list los that starts
;        after the first occurence of the symbol s.
(define drop-until
  (lambda (s los)
    (if (null? los)
      '()
      (if (eqv? (car los) s)
        (cdr los)
        (drop-until s (cdr los))))))
