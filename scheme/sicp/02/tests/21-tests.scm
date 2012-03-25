(require rackunit rackunit/text-ui)
(load "../21.scm")

(define sicp-2.21-tests
  (test-suite
    "Tests for SICP exercise 2.21"

    (check-equal? (square-list-1 (list 1 2 3 4)) (list 1 4 9 16))
    (check-equal? (square-list-2 (list 1 2 3 4)) (list 1 4 9 16))
))

(run-tests sicp-2.21-tests)
