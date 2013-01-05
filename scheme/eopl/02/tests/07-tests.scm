(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../07.scm")

(define eopl-2.07-tests
  (test-suite
    "Tests for EOPL exercise 2.07"

    (check-exn (regexp "Variable b not found in environment: \\(extend-env a 1 \\(empty-env\\)\\)")
               (lambda () (apply-env (extend-env 'a 1 (empty-env)) 'b)))
))

(exit (run-tests eopl-2.07-tests))
