(module env-proc-test scheme
  (require (planet schematics/schemeunit:3))
  (require (planet schematics/schemeunit:3/text-ui))
  (require "env-proc.ss")

  (define env-proc-tests
    (test-suite
      "Procedural representation of environment"
    
    (check-pred empty-env? (empty-env))
    (check-false (empty-env? (extend-env 'a 1 (empty-env))))
    (check-exn exn? (lambda () (apply-env 'a (empty-env))))
    (check-equal? (apply-env 'a (extend-env 'a 1 (empty-env))) 1)
    (check-equal? (apply-env 'a (extend-env 'b 2 (extend-env 'a 1 (empty-env)))) 1)
  ))

  (run-tests env-proc-tests)
)
