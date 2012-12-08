(require rackunit rackunit/text-ui)
(load "../21.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.21-tests
  (test-suite
    "Tests for SICP exercise 4.21"

    (check-equal? 3628800
                  (run '((lambda (n)
                           ((lambda (fact)
                              (fact fact n))
                            (lambda (ft k)
                              (if (= k 1)
                                1
                                (* k (ft ft (- k 1)))))))
                         10)))

    (check-equal? (y-fibonacci 3) 2)
    (check-equal? (y-fibonacci 4) 3)
    (check-equal? (y-fibonacci 5) 5)
    (check-equal? (y-fibonacci 6) 8)
    (check-equal? (y-fibonacci 7) 13)

    (check-true (f 0))
    (check-false (f 1))
    (check-true (f 2))
    (check-false (f 3))
    (check-true (f 4))
    (check-false (f 5))
))

(run-tests sicp-4.21-tests)
