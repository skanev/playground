(require rackunit rackunit/text-ui)
(load "../59.scm")

(define sicp-3.59-tests
  (test-suite
    "Tests for SICP exercise 3.59"

    (check-equal? (stream-take (integrate-series integers) 6)
                  '(1 1 1 1 1 1))

    (check-equal? (stream-take cosine-series 6)
                  (list 1 0 (/ -1 2) 0 (/ 1 24) 0))
    (check-equal? (stream-take sine-series 6)
                  (list 0 1 0 (/ -1 6) 0 (/ 1 120)))
))

(run-tests sicp-3.59-tests)
