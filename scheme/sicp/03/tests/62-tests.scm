(require rackunit rackunit/text-ui)
(load "../62.scm")

(define sicp-3.62-tests
  (test-suite
    "Tests for SICP exercise 3.62"

    (check-exn exn? (lambda () (div-series ones (stream-cons 0 ones))))

    (check-equal? (stream-take tangent-series 6)
                  (list 0 1 0 (/ 1 3) 0 (/ 2 15)))
))

(run-tests sicp-3.62-tests)
