(require rackunit rackunit/text-ui)
(load "../07.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.07-tests
  (test-suite
    "Tests for SICP exercise 4.07"

    (check-equal? (run '(let* ((x 3)
                               (y (+ x 2))
                               (z (+ x y 5)))
                          (* x z)))
                  39)
))

(run-tests sicp-4.07-tests)
