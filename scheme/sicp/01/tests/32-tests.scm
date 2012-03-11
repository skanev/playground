(require rackunit rackunit/text-ui)
(load "../32.scm")

(define (increment n)
  (+ n 1))
(define (one-over-pow-of-2 n)
  (/ 1 (expt 2 n)))

(define sicp-1.32-tests
  (test-suite
    "Tests for SICP exercise 1.32"

    (check-equal? (accumulate * 1 identity 1 increment 5) 120)
    (check-equal? (product identity 1 increment 5) 120)
    (check-equal? (sum identity 1 increment 10) 55)
    (check-= (accumulate + 0 one-over-pow-of-2 0 increment 1000) 2 0.001)

    (check-equal? (i-accumulate * 1 identity 1 increment 5) 120)
    (check-equal? (i-product identity 1 increment 5) 120)
    (check-equal? (i-sum identity 1 increment 10) 55)
    (check-= (i-accumulate + 0 one-over-pow-of-2 0 increment 1000) 2 0.001)
))

(run-tests sicp-1.32-tests)
