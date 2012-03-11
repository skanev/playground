(require rackunit rackunit/text-ui)
(load "../31.scm")

(define sicp-1.31-tests
  (test-suite
    "Tests for SICP exercise 1.31"

    (check-equal? (factorial 1) 1)
    (check-equal? (factorial 5) 120)

    (check-= (approximated-pi 1000) (/ 3.14 4) 0.001)

    (check-equal? (i-factorial 1) 1)
    (check-equal? (i-factorial 5) 120)

    (check-= (i-approximated-pi 1000) (/ 3.14 4) 0.001)
))

(run-tests sicp-1.31-tests)
