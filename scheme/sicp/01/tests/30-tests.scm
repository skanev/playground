(require rackunit rackunit/text-ui)
(load "../30.scm")

(define sicp-1.30-tests
  (test-suite
    "Tests for SICP exercise 1.30"

    (check-equal? (sum (lambda (x) x) 1 (lambda (x) (+ 1 x)) 100) 5050)
    (check-equal? (sum (lambda (x) x) 0 (lambda (x) (+ 1 x)) 1) 1)
    (check-equal? (sum (lambda (x) x) 0 (lambda (x) (+ 1 x)) 0) 0)
    (check-equal? (sum (lambda (x) x) 0 (lambda (x) (+ 2 x)) 10) 30)

    (check-= (sum (lambda (x) (/ 1 (expt 2 x))) 1.0 (lambda (x) (+ 1 x)) 100) 1.0 0.01)
))

(run-tests sicp-1.30-tests)
