(module ribcage-test scheme
  (require (planet schematics/schemeunit:3))
  (require (planet schematics/schemeunit:3/text-ui))
  (require "ribcage.ss")


  (define ribcage-tests
    (test-suite
      "Ribcage environment representation"

      (check-equal? (extend-env* '(a b) '(1 2) (empty-env)) '(((a b) (1 2))))
      (check-equal? (extend-env* '(a) '(1) (extend-env* '(b) '(2) (empty-env))) '(((a) (1)) ((b) (2))))
      (check-equal? (extend-env 'a 1 (empty-env)) (extend-env* '(a) '(1) (empty-env)))

      (check-equal? (apply-env 'a (extend-env* '(a b) '(1 2) (empty-env))) 1)
      (check-equal? (apply-env 'a (extend-env* '(b) '(2) (extend-env* '(a) '(1) (empty-env)))) 1)
      (check-equal? (apply-env 'a (extend-env* '(b a) '(2 1) (extend-env* '(a) '(3) (empty-env)))) 1)
  ))

  (run-tests ribcage-tests)
)
