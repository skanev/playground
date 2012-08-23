(require rackunit rackunit/text-ui)
(load "../75.scm")

(define sicp-2.75-tests
  (test-suite
    "Tests for SICP exercise 2.75"

    (check-equal? (magnitute (make-from-mag-ang 3 4)) 3)
    (check-equal? (angle (make-from-mag-ang 3 4)) 4)
    (check-= (real-part (make-from-mag-ang 2 (acos 1))) 2 0.001)
    (check-= (imag-part (make-from-mag-ang 2 (asin 1))) 2 0.001)
))

(run-tests sicp-2.75-tests)
