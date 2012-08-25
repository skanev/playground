(require rackunit rackunit/text-ui)
(load "../80.scm")

(define sicp-2.80-tests
  (test-suite
    "Tests for SICP exercise 2.80"

    (check-true (=zero? (make-scheme-number 0)))
    (check-false (=zero? (make-scheme-number 1)))

    (check-true (=zero? (make-rational 0 1)))
    (check-true (=zero? (make-rational 0 2)))
    (check-false (=zero? (make-rational 1 2)))

    (check-true (=zero? (make-complex-from-real-imag 0 0)))
    (check-false (=zero? (make-complex-from-real-imag 0 1)))

    (check-true (=zero? (make-complex-from-mag-ang 0 1)))
    (check-false (=zero? (make-complex-from-mag-ang 1 0)))
))

(run-tests sicp-2.80-tests)
