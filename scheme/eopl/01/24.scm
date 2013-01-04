; EOPL exercise 1.24
;
; (every? pred lst) returns #f if any element of lst fails to satisfy pred,
; and returns #t otherwise.
;
; > (every? number? '(a b c 3 e))
; #f
; > (every? number? '(1 2 3 5 4))
; #t

(define (every? pred lst)
  (cond ((null? lst) #t)
        ((pred (car lst)) (every? pred (cdr lst)))
        (#t #f)))
