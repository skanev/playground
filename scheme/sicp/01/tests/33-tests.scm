(require rackunit rackunit/text-ui)
(load "../33.scm")

(define sicp-1.33-tests
  (test-suite
    "Tests for SICP exercise 1.33"

    (check-equal? (sum-of-prime-squares 2 10) 87)
    (check-equal? (sum-of-prime-squares 10 15) 290)

    (check-equal? (product-of-relative-primes-to 10) 189)
    (check-equal? (product-of-relative-primes-to 12) 385)
))

(run-tests sicp-1.33-tests)
