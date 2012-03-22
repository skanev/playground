(require rackunit rackunit/text-ui)
(load "../07.scm")

(define sicp-2.07-tests
  (test-suite
    "Tests for SICP exercise 2.07"

    (check-equal? (lower-bound (make-interval 4 5)) 4)
    (check-equal? (upper-bound (make-interval 4 5)) 5)
))

(run-tests sicp-2.07-tests)
