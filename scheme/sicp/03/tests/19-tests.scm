(require rackunit rackunit/text-ui)
(load "../19.scm")

(define sicp-3.19-tests
  (test-suite
    "Tests for SICP exercise 3.19"

    (check-true (has-cycle? (make-cycle '(a b c d))))
    (check-true (has-cycle? (make-cycle '(a b c d e))))

    (check-false (has-cycle? '()))
    (check-false (has-cycle? '(a)))
    (check-false (has-cycle? '(a b)))
    (check-false (has-cycle? '(a b c)))
    (check-false (has-cycle? '(a b c d)))
))

(run-tests sicp-3.19-tests)
