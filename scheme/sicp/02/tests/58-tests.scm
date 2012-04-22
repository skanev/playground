(require rackunit rackunit/text-ui)
(load "../58.scm")

(define sicp-2.58-tests
  (test-suite
    "Tests for SICP exercise 2.58"

    (check-equal? (deriv '(x * x) 'x)
                         '(x + x))

    (check-equal? (deriv '(x + (3 * (x + (y + 2)))) 'x)
                  4)
))

(run-tests sicp-2.58-tests)
