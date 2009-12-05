(module alt-repr-one-test scheme
  (require (planet schematics/schemeunit:3)
           (planet schematics/schemeunit:3/text-ui)
           "alt-repr-one.ss")

  (define alt-repr-one-tests
    (test-suite
      "alternative representation one"

      (check-equal? (empty-env) '(() ()))
      (check-equal? (extend-env 'a 1 (extend-env 'b 2 (empty-env))) '((a b) (1 2)))

      (check-equal? (apply-env 'one (extend-env 'one 1 (empty-env))) 1 "Simply apply-env")
      (check-equal? (apply-env 'one (extend-env 'one 1 (extend-env 'two 2 (extend-env 'one 3 (empty-env))))) 1 "extend-env overwrites names")
      
      (check-equal? (apply-env 'three (extend-env 'one 1 (extend-env 'two 2 (extend-env 'three 3 (empty-env))))) 3 "deep apply-env")
    )
  )

  (run-tests alt-repr-one-tests)
)

