; SICP exercise 2.54
;
; Two lists are said to be equal? if they contain equal elements arranged in
; the same order. For example,
;
; (equal? '(this is a list) '(this is a list))
;
; is there, but
;
; (equal? '(this is a list) '(this (is a) list))
;
; is false. To be more precise, we can define equal? recursively in terms of
; the basic eq? equality of symbols by saying that a and b are eq?, or if they
; are both lists such that (car a) is equal? to (car b) and (cdr a) is equal?
; to (cdr b). Using this idea, implement equal? as a procedure.

(define (equal2? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((and (pair? a) (pair? b))
         (and (equal2? (car a) (car b))
              (equal2? (cdr a) (cdr b))))
        (else (eq? a b))))
