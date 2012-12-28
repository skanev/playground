; SICP exercise 5.41
;
; Write a procedure find-variable that takes as arguments a variable and a
; compile-time environment and returns the lexical address of the variable
; with respect to that environment. For example, in the program fragment shown
; above, the compile-time environment during the compilation of expression
; <e1> is ((y z) (a b c d e) (x y)). find-variable should produce
;
; (find-variable 'c '((y z) (a b c d e) (x y)))
; (1 2)
;
; (find-variable 'x '((y z) (a b c d e) (x y)))
; (2 0)
;
; (find-variable 'w '((y z) (a b c d e) (x y)))
; not-found

(define (find-variable var env)
  (define (loop frame position vars env)
    (cond ((and (null? vars) (null? (cdr env))) 'not-found)
          ((null? vars) (loop (+ frame 1) 0 (cadr env) (cdr env)))
          ((eq? (car vars) var) (list frame position))
          (else (loop frame (+ position 1) (cdr vars) env))))
  (if (null? env)
      'not-found
      (loop 0 0 (car env) env)))
