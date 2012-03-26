(require rackunit rackunit/text-ui)
(load "../27.scm")

(define sicp-2.27-tests
  (test-suite
    "Tests for SICP exercise 2.27"

    (check-equal? (deep-reverse '((1 2) (3 4))) '((4 3) (2 1)))
))

(run-tests sicp-2.27-tests)
