(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../16.scm")

(define eopl-2.16-tests
  (test-suite
    "Tests for EOPL exercise 2.16"

    (check-equal? (lambda-exp 'a (var-exp 'b))
                  '(lambda a b))

    (check-true (var-exp? (var-exp 'a)))
    (check-equal? (var-exp->var (var-exp 'a)) 'a)

    (check-true (lambda-exp? (lambda-exp 'a (var-exp 'b))))
    (check-equal? (lambda-exp->bound-var (lambda-exp 'a (var-exp 'b))) 'a)
    (check-equal? (lambda-exp->body (lambda-exp 'a (var-exp 'b))) (var-exp 'b))

    (check-true (app-exp? (app-exp (var-exp 'a) (var-exp 'b))))
    (check-equal? (app-exp->rator (app-exp (var-exp 'a) (var-exp 'b))) (var-exp 'a))
    (check-equal? (app-exp->rand (app-exp (var-exp 'a) (var-exp 'b))) (var-exp 'b))

    (check-false (app-exp? (lambda-exp 'a (var-exp 'b))))
    (check-false (lambda-exp? (app-exp (var-exp 'a) (var-exp 'b))))
))

(exit (run-tests eopl-2.16-tests))
