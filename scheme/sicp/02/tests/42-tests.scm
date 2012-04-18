(require rackunit rackunit/text-ui)
(load "../42.scm")

(define sicp-2.42-tests
  (test-suite
    "Tests for SICP exercise 2.42"

    (check-true (safe? 2 '((1 1) (3 2))))
    (check-false (safe? 2 '((1 1) (1 2))))

    (check-true (safe? 2 '((3 1) (1 2))))
    (check-false (safe? 2 '((3 1) (2 2))))
    (check-false (safe? 2 '((1 1) (2 2))))

    (check-true (all? zero? '(0 0 0)))
    (check-false (all? zero? '(0 1 0)))
    (check-true (all? zero? '()))

    (check-equal? 92 (length (queens 8)))
))

(run-tests sicp-2.42-tests)
