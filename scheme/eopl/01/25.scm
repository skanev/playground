; EOPL exercise 1.25
;
; (exists? number? '(a b c 3 e)) returns #t if any element of lst satisfies
; pred, and returns #f otherwise.
;
; > (exists? number? '(a b c 3 e))
; #t
; > (exists? number? '(a b c d e))
; #f

(define (exists? pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (#t (exists? pred (cdr lst)))))
