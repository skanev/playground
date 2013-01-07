; EOPL exercise 2.22
;
; Using define-datatype, implement the stack data type of exercise 2.4.

(define-datatype stack stack?
  (empty-stack)
  (push (var (lambda (x) #t))
        (frame stack?)))

(define (empty-stack? st)
  (cases stack st
    (empty-stack () #t)
    (push (var frame) #f)))

(define (pop st)
  (cases stack st
    (empty-stack () (eopl:error 'pop "Empty stack."))
    (push (var frame) frame)))
