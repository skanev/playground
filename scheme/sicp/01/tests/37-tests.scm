(require rackunit rackunit/text-ui)
(load "../37.scm")

(define one (lambda (i) 1.0))
(define two (lambda (i) 2.0))

(define sicp-1.37-tests
  (test-suite
    "Tests for SICP exercise 1.37"

    (check-equal? (cont-frac one one 1) 1.0)
    (check-equal? (cont-frac one two 1) 0.5)
    (check-equal? (cont-frac two one 1) 2.0)
    (check-= (cont-frac one one 11) 0.6180 0.00006)

    (check-equal? (cont-frac-i one one 1) 1.0)
    (check-equal? (cont-frac-i one two 1) 0.5)
    (check-equal? (cont-frac-i two one 1) 2.0)
    (check-= (cont-frac-i one one 11) 0.6180 0.00006)
))

(run-tests sicp-1.37-tests)
