(require rackunit rackunit/text-ui)
(load "../08.scm")

(define sicp-2.08-tests
  (test-suite
    "Tests for SICP exercise 2.08"

    (check-equal? (sub-interval (make-interval 20 25)
                                (make-interval 1 2))
                  (make-interval 18 24))

    (check-equal? (sub-interval (make-interval 10 12)
                                (make-interval -3 -1))
                  (make-interval 11 15))
))

(run-tests sicp-2.08-tests)
