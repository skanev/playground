(module a-list-env-test scheme
  (require (planet schematics/schemeunit:3)
           (planet schematics/schemeunit:3/text-ui)
           "a-list-env.ss")


  (define a-list-env-tests
    (test-suite
      "a-list environment representation"

      (check-equal? (empty-env) '() "Empty environment")
      (check-pred empty-env? (empty-env) "empty-env? for empty environment")
      (check-false (empty-env? (extend-env 'one 1 (empty-env))) "empty-env? for non-empty environment")

      (check-equal? (extend-env 'one 1 (empty-env)) '((one 1)) "Simple extend-env")
      (check-equal? (apply-env 'one (extend-env 'one 1 (empty-env))) 1 "Simply apply-env")
      (check-equal? (apply-env 'one (extend-env 'one 1 (extend-env 'two 2 (extend-env 'one 3 (empty-env))))) 1 "extend-env overwrites names")
      (check-exn exn? (lambda () (apply-env 'foo (empty-env))) "apply-env raises an error when no variable found")

      (check-true (has-binding? 'one (extend-env 'one 1 (empty-env))))
      (check-false (has-binding? 'two (extend-env 'one 1 (empty-env))))

      (check-equal? (apply-env 'two (extend-env* '(one two) '(1 2) (empty-env))) 2)
      (check-equal? (apply-env 'one (extend-env* '(one two one) '(1 2 3) (empty-env))) 1)
      (check-false (has-binding? 'three (extend-env* '(one two) '(1 2) (empty-env))) 1)
    )
  )

  (run-tests a-list-env-tests)
)
