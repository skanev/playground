(require rackunit rackunit/text-ui)
(load "../79.scm")

(define sicp-2.79-tests
  (test-suite
    "Tests for SICP exercise 2.79"

    (check-true (equ? (make-scheme-number 1) (make-scheme-number 1)))
    (check-false (equ? (make-scheme-number 1) (make-scheme-number 2)))

    (check-true (equ? (make-rational 1 2) (make-rational 1 2)))
    (check-true (equ? (make-rational 1 2) (make-rational 2 4)))
    (check-false (equ? (make-rational 1 2) (make-rational 2 2)))
    (check-false (equ? (make-rational 1 2) (make-rational 1 1)))

    (check-true (equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2)))
    (check-true (equ? (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 1 2)))
    (check-false (equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 1)))
    (check-false (equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 2 2)))
    (check-false (equ? (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 1 1)))
    (check-false (equ? (make-complex-from-mag-ang 1 2) (make-complex-from-mag-ang 2 2)))
))

(run-tests sicp-2.79-tests)
