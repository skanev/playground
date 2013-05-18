(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../29.scm")
(load-relative "helpers/proc.scm")

(define eopl-3.29-tests
  (test-suite
    "Tests for EOPL exercise 3.29"

    (check-equal? (run the-hypothetical-program) 2)
))

(exit (run-tests eopl-3.29-tests))
