(module env-proc-has-binding-test scheme
  (require (planet schematics/schemeunit:3))
  (require (planet schematics/schemeunit:3/text-ui))
  (require "env-proc-has-binding.ss")

  (define env-proc-has-binding-tests
    (test-suite
      "Procedural representation of environment with has-binding?"
    
    (check-pred empty-env? (empty-env))
    (check-false (empty-env? (extend-env 'a 1 (empty-env))))
    (check-exn exn? (lambda () (apply-env 'a (empty-env))))
    (check-equal? (apply-env 'a (extend-env 'a 1 (empty-env))) 1)
    (check-equal? (apply-env 'a (extend-env 'b 2 (extend-env 'a 1 (empty-env)))) 1)

    (check-false (has-binding? 'a (empty-env)))
    (check-false (has-binding? 'b (extend-env 'a 1 (empty-env))))
    (check-true (has-binding? 'a (extend-env 'a 1 (empty-env))))
    (check-true (has-binding? 'b (extend-env 'a 1 (extend-env 'b 2 (empty-env)))))
  ))

  (run-tests env-proc-has-binding-tests)
)
