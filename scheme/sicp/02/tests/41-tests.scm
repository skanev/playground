(require rackunit rackunit/text-ui)
(load "../41.scm")

(define sicp-2.41-tests
  (test-suite
    "Tests for SICP exercise 2.41"

    (check-equal? (triples-sum 5 9) '((1 3 5) (2 3 4)))
    (check-equal? (triples-sum 5 10) '((1 4 5) (2 3 5)))
))

(run-tests sicp-2.41-tests)
