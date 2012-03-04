(require rackunit rackunit/text-ui)
(load "../18.scm")

(define sicp-1.18-tests
  (test-suite
    "Tests for SICP exercise 1.18"

    (check-equal? (** 5 1) 5)
    (check-equal? (** 5 2) 10)
    (check-equal? (** 5 3) 15)
    (check-equal? (** 5 4) 20)
    (check-equal? (** 5 5) 25)

    (check-equal? (** 1 2) 2)
    (check-equal? (** 2 2) 4)
    (check-equal? (** 3 2) 6)
    (check-equal? (** 4 2) 8)
    (check-equal? (** 5 2) 10)
))

(run-tests sicp-1.18-tests)
