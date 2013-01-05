(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../08.scm")

(define eopl-2.08-tests
  (test-suite
    "Tests for EOPL exercise 2.08"

    (check-true (empty-env? (empty-env)))
    (check-false (empty-env? (extend-env 'a 1 (empty-env))))
))

(exit (run-tests eopl-2.08-tests))
