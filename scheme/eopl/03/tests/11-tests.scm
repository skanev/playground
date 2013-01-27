(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../11.scm")

(define (run code)
  (eval* code (empty-env)))

(define (expval->schemeval val)
  (cases expval val
    (num-val (num) num)
    (bool-val (bool) bool)))

(define (schemeval->expval val)
  (cond ((number? val) (num-val val))
        ((boolean? val) (bool-val val))
        (else (error 'schemeval->expval "Don't know how to convert ~s" val))))

(define (eval* code env)
  (let* ((expr (scan&parse code))
         (result (value-of expr env)))
    (expval->schemeval result)))

(define (env vars vals)
  (extend-env* vars
               (map schemeval->expval vals)
               (empty-env)))

(define eopl-3.11-tests
  (test-suite
    "Tests for EOPL exercise 3.11"

    (check-equal? (run "+(1, +(2, 3))") 6)

    (check-equal? (run "+(4, 5)") 9)
    (check-equal? (run "*(7, 4)") 28)
    (check-equal? (run "/(10, 3)") 3)

    (check-equal? (run "42") 42)
    (check-equal? (eval* "x" (env '(x) '(10))) 10)
    (check-equal? (eval* "-(x, 7)" (env '(x) '(10))) 3)

    (check-equal? (run "% Comment\n 1") 1)

    (check-equal? (run "let x = 1 in x") 1)
    (check-equal? (run "let x = 1 in let x = 2 in x") 2)
    (check-equal? (run "let x = 1 in let y = 2 in x") 1)

    (check-equal? (run "let x = 7                  % This is a comment
                        in let y = 2               % This is another comment
                           in let y = let x = -(x, 1) in -(x, y)
                              in -(-(x, 8),y)")
                  -5)
))

(exit (run-tests eopl-3.11-tests))
