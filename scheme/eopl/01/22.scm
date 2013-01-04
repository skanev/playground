; EOPL exercise 1.22
;
; (filter-in pred lst) returns the list of those elements in lst that satisfy
; the predicate pred.
;
; > (filter-in number? '(a 2 (1 3) b 7))
; (2 7)
; > (filter-in symbol? '(a (b c) 17 foo))
; (a foo)

(define (filter-in pred lst)
  (if (null? lst)
    '()
    (if (pred (car lst))
      (cons (car lst) (filter-in pred (cdr lst)))
      (filter-in pred (cdr lst)))))
