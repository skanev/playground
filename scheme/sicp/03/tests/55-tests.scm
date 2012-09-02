(require rackunit rackunit/text-ui)
(load "../55.scm")

(define sicp-3.55-tests
  (test-suite
    "Tests for SICP exercise 3.55"

    (check-equal? (stream-take (partial-sums integers) 5)
                  '(1 3 6 10 15))
))

(run-tests sicp-3.55-tests)
