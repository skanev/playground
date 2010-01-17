(module lc-exp-datatype-test scheme
  (require (planet schematics/schemeunit:3)
           (planet schematics/schemeunit:3/text-ui)
           "lc-exp-datatype.ss")

  (define lc-exp-datatype-tests
    (test-suite
      "lc-exp with datatype"

      (check-exn exn? (lambda () (var-exp 'lambda)))
      (check-exn exn? (lambda () (lambda-exp 'lambda (var-exp 'foo))))
      (check-pred lc-exp? (lambda-exp 'foo (var-exp 'foo)))
      (check-pred lc-exp? (var-exp 'foo))

  ))

  (run-tests lc-exp-datatype-tests)
)

