(require rackunit rackunit/text-ui)
(load "../43.scm")

(define (square x)
  (* x x))

(define sicp-1.43-tests
  (test-suite
    "Tests for SICP exercise 1.43"

    (check-equal? ((repeated square 2) 5) 625)
))

(run-tests sicp-1.43-tests)
