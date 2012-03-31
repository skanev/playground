(require rackunit rackunit/text-ui)
(load "../33.scm")

(define sicp-2.33-tests
  (test-suite
    "Tests for SICP exercise 2.33"

    (check-equal? (map (lambda (x) (* x x)) '(1 2 3 4)) '(1 4 9 16))
    (check-equal? (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
    (check-equal? (length '(1 2 3)) 3)
))

(run-tests sicp-2.33-tests)
