; EOPL exercise 2.12
;
; Implement the stack data type of exercise 2.4 using a procedural
; representation.

; I just don't get this. Anyway.

(define (empty-stack? stack)
  (stack (lambda (top rest) (null? rest))))

(define (empty-stack)
  (lambda (proc)
    (proc #f '())))

(define (push val stack)
  (lambda (proc)
    (proc val stack)))

(define (pop stack)
  (stack (lambda (top rest) rest)))
