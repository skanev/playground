(require rackunit rackunit/text-ui)
(load "../42.scm")

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define sicp-1.42-tests
  (test-suite
    "Tests for SICP exercise 1.42"

    (check-equal? ((compose square inc) 6) 49)
))

(run-tests sicp-1.42-tests)
