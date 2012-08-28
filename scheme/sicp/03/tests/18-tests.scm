(require rackunit rackunit/text-ui)
(load "../18.scm")

(define sicp-3.18-tests
  (test-suite
    "Tests for SICP exercise 3.18"

    (check-true (has-cycle? (make-cycle '(a b c d))))
    (check-false (has-cycle? '(a b c d)))
))

(run-tests sicp-3.18-tests)
