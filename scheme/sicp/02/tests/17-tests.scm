(require rackunit rackunit/text-ui)
(load "../17.scm")

(define sicp-2.17-tests
  (test-suite
    "Tests for SICP exercise 2.17"

    (check-equal? (last-pair (list 23 72 149 34)) (list 34))
    (check-equal? (last-pair (list 34)) (list 34))
))

(run-tests sicp-2.17-tests)
