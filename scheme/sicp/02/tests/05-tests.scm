(require rackunit rackunit/text-ui)
(load "../05.scm")

(define sicp-2.05-tests
  (test-suite
    "Tests for SICP exercise 2.05"

    (check-equal? (car (cons 5 7)) 5)
    (check-equal? (cdr (cons 5 7)) 7)

    (check-equal? (car (cons 0 7)) 0)
    (check-equal? (cdr (cons 5 0)) 0)
))

(run-tests sicp-2.05-tests)
