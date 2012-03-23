(require rackunit rackunit/text-ui)
(load "../12.scm")

(define sicp-2.12-tests
  (test-suite
    "Tests for SICP exercise 2.12"

    (check-equal? (percent (make-interval 95 105)) 5)
    (check-equal? (make-center-percent 100 5) (make-interval 95 105))
    (check-equal? (percent (make-center-percent 100 5)) 5)
))

(run-tests sicp-2.12-tests)
