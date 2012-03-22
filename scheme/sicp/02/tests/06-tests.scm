(require rackunit rackunit/text-ui)
(load "../06.scm")

(define (double x)
  (+ x x))

(define sicp-2.06-tests
  (test-suite
    "Tests for SICP exercise 2.06"

    (check-equal? ((one double) 2) 4)
    (check-equal? ((two double) 2) 8)

    (check-equal? (((add one two) double) 2) 16)
))

(run-tests sicp-2.06-tests)
