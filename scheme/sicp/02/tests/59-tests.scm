(require rackunit rackunit/text-ui)
(load "../59.scm")

(define sicp-2.59-tests
  (test-suite
    "Tests for SICP exercise 2.59"

    (check-equal? (union-set '(1 2) '(3 4))
                  '(1 2 3 4))

    (check-equal? (union-set '(1 2 3) '(2 4))
                  '(1 3 2 4))
))

(run-tests sicp-2.59-tests)
