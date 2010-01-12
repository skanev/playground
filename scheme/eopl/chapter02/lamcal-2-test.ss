(module lamcal-2-test scheme
  (require (planet schematics/schemeunit:3))
  (require (planet schematics/schemeunit:3/text-ui))
  (require "lamcal-2.ss")

  (define lamcal-2-tests
    (test-suite
      "Simple lambda calculus"

      (check-equal? (var-exp 'foo) 'foo)
      (check-pred var-exp? 'foo)
      (check-pred var-exp? (var-exp 'foo))
      (check-equal? (var-exp->var 'foo) 'foo)
      (check-equal? (var-exp->var (var-exp 'foo)) 'foo)

      (check-equal? (lambda-exp 'bar (var-exp 'foo)) '(lambda bar foo))
      (check-pred lambda-exp? '(lambda bar foo))
      (check-pred lambda-exp? (lambda-exp 'bar (var-exp 'foo)))
      (check-false (lambda-exp? (var-exp 'foo)))
      (check-equal? (lambda-exp->bound-var (lambda-exp 'foo (var-exp 'bar))) 'foo)
      (check-equal? (lambda-exp->body (lambda-exp 'foo (var-exp 'bar))) (var-exp 'bar))

      (check-equal? (app-exp 'foo 'bar) '(foo bar))
      (check-pred app-exp? (app-exp 'foo 'bar))
      (check-pred app-exp? (app-exp (lambda-exp 'one (var-exp 'one)) 'two))
      (check-false (app-exp? (lambda-exp 'one (var-exp 'one))))
      (check-false (app-exp? (var-exp 'one)))
      (check-equal? (app-exp->rator (app-exp (var-exp 'foo) (var-exp 'bar))) 'foo)
      (check-equal? (app-exp->rand (app-exp (var-exp 'foo) (var-exp 'bar))) 'bar)
  ))

  (run-tests lamcal-2-tests)
)
