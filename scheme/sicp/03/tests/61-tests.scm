(require rackunit rackunit/text-ui)
(load "../61.scm")

(define sicp-3.61-tests
  (test-suite
    "Tests for SICP exercise 3.61"

    (check-equal? (stream-take (mul-series cosine-series
                                           (invert-unit-series cosine-series))
                               6)
                  '(1 0 0 0 0 0))
))

(run-tests sicp-3.61-tests)
