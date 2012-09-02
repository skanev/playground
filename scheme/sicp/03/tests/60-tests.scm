(require rackunit rackunit/text-ui)
(load "../60.scm")

(define sicp-3.60-tests
  (test-suite
    "Tests for SICP exercise 3.60"

    (check-equal? (stream-take (add-streams (mul-series sine-series sine-series)
                                            (mul-series cosine-series cosine-series))
                               6)
                  '(1 0 0 0 0 0))
))

(run-tests sicp-3.60-tests)
