(module env-with-datatype-test scheme
  (require (planet schematics/schemeunit:3)
           (planet schematics/schemeunit:3/text-ui)
           "env-with-datatype.ss")

  (define env-with-datatype-tests
    (test-suite
      "Environment with define-datatype"

    (check-equal? (apply-env (extend-env 'foo 42 (empty-env)) 'foo) 42 "apply-env")
    (check-true (has-binding? (extend-env 'foo 42 (empty-env)) 'foo) "has-binding? -> true")
    (check-false (has-binding? (empty-env) 'foo) "has-binding -> false")
    (check-exn exn? (lambda () (apply-env (empty-env) 'foo)) "(apply-env 'unbinding) raises error")
  ))

  (run-tests env-with-datatype-tests)
)
