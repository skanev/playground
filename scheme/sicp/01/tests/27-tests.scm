(require rackunit rackunit/text-ui)
(load "../27.scm")

(define sicp-1.27-tests
  (test-suite
    "Tests for SICP exercise 1.27"

    (check-true (carmichael? 561))
    (check-true (carmichael? 1105))
    (check-true (carmichael? 1729))
    (check-true (carmichael? 2465))
    (check-true (carmichael? 2821))
    (check-true (carmichael? 6601))

    (check-false (carmichael? 27))
    (check-false (carmichael? 1001))
))

(run-tests sicp-1.27-tests)
