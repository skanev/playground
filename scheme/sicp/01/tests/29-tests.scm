(require rackunit rackunit/text-ui)
(load "../29.scm")

(define sicp-1.29-tests
  (test-suite
    "Tests for SICP exercise 1.29"

    (check-= (integral cube 0 1 0.001) 0.25 0.001)
    (check-= (integral (lambda (x) x) 0 1 0.001) 0.5 0.001)
    
    (check-= (simpson-integral cube 0 1 100) 0.25 0.01)
    (check-= (simpson-integral (lambda (x) x) 0 1 100) 0.5 0.01)
    (check-= (simpson-integral (lambda (x) x) 0 1 1000) 0.5 0.001)
))

(run-tests sicp-1.29-tests)
