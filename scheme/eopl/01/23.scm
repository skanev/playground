; EOPL exercise 1.23
;
; (list-index pred lst) returns the 0-based position of the first element of
; lst that satisfies the predicate pred. If no element of lst satisfies the
; predicate, then list-index returns #f.
;
; > (list-index number? '(a 2 (1 3) b 7))
; 1
; > (list-index symbol? '(a (b c) 17 foo))
; 0
; > (list-index symbol? '(1 2 (a b) 3))
; #f

(define (list-index pred lst)
  (define (iter counter items)
    (cond ((null? items) #f)
          ((pred (car items)) counter)
          (#t (iter (+ counter 1) (cdr items)))))
  (iter 0 lst))

