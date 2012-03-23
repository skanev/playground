(require rackunit rackunit/text-ui)
(load "../11.scm")

(define sicp-2.11-tests
  (test-suite
    "Tests for SICP exercise 2.11"

    (check-equal? (mul-interval (make-interval 1 2)
                                (make-interval 3 4))
                  (make-interval 3 8))

    (check-equal? (mul-interval (make-interval 1 2)
                                (make-interval -3 4))
                  (make-interval -6 8))

    (check-equal? (mul-interval (make-interval 1 2)
                                (make-interval -4 -3))
                  (make-interval -8 -3))

    (check-equal? (mul-interval (make-interval -1 2)
                                (make-interval 3 4))
                  (make-interval -4 8))

    (check-equal? (mul-interval (make-interval -1 2)
                                (make-interval -3 4))
                  (make-interval -6 8))
    (check-equal? (mul-interval (make-interval -4 2)
                                (make-interval -3 4))
                  (make-interval -16 12))
    (check-equal? (mul-interval (make-interval -2 2)
                                (make-interval -3 2))
                  (make-interval -6 6))
    (check-equal? (mul-interval (make-interval -2 2)
                                (make-interval -2 3))
                  (make-interval -6 6))

    (check-equal? (mul-interval (make-interval -1 2)
                                (make-interval -4 -3))
                  (make-interval -8 4))

    (check-equal? (mul-interval (make-interval -2 -1)
                                (make-interval 3 4))
                  (make-interval -8 -3))

    (check-equal? (mul-interval (make-interval -2 -1)
                                (make-interval -3 4))
                  (make-interval -8 6))

    (check-equal? (mul-interval (make-interval -2 -1)
                                (make-interval -4 -3))
                  (make-interval 3 8))
))

(run-tests sicp-2.11-tests)
