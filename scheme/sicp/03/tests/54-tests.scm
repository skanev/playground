(require rackunit rackunit/text-ui)
(load "../54.scm")

(define sicp-3.54-tests
  (test-suite
    "Tests for SICP exercise 3.54"

    (check-equal? (stream-take factorials 6)
                  '(1 2 6 24 120 720))
))

(run-tests sicp-3.54-tests)
