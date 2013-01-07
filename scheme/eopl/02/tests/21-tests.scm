(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../21.scm")

(define eopl-2.21-tests
  (test-suite
    "Tests for EOPL exercise 2.21"

    (check-true (has-binding? (extend-env 'a 1 (empty-env)) 'a))
    (check-true (has-binding? (extend-env 'a 1 (extend-env 'b 2 (empty-env))) 'b))

    (check-false (has-binding? (empty-env) 'a))
    (check-false (has-binding? (extend-env 'a 1 (extend-env 'b 2 (empty-env))) 'c))
))

(exit (run-tests eopl-2.21-tests))
