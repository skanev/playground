; EOPL exercise 2.30
;
; The procedure parse-expression as defined above is fragile: it does not
; detect several possible syntactic errors, such as (a b c), and aborts with
; inappropriate error messages for other expressions, such as (lambda). Modify
; it so that it is robust, accepting any s-exp and issuing an appropriate
; error message if the s-exp does not represent a lambda-calculus expression.

(load-relative "23.scm")

(define (parse datum)
  (cond ((symbol? datum)
         (when (eqv? datum 'lambda)
           (eopl:error 'parse "lambda is not a valid identifier"))
         (var-exp datum))
        ((and (pair? datum)
              (eqv? (car datum) 'lambda))
         (unless (= (length datum) 3)
           (eopl:error 'parse "lambda requires two components. given: ~s" datum))
         (when (symbol? (cadr datum))
           (eopl:error 'parse "lambda requires an arglist. given: ~s" (cadr datum)))
         (unless (= (length (cadr datum)) 1)
           (eopl:error 'parse "lambda requires exactly one argument. given: ~s" (cadr datum)))
         (lambda-exp (car (cadr datum))
                     (parse (caddr datum))))
        ((pair? datum)
         (unless (= (length datum) 2)
           (eopl:error 'parse "application requires two components. given: ~s" datum))
         (app-exp (parse (car datum))
                  (parse (cadr datum))))
        (else (eopl:error 'parse "Invalid syntax: ~s" datum))))


