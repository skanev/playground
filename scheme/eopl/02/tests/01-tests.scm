(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../01.scm")

(define eopl-2.01-tests
  (test-suite
    "Tests for EOPL exercise 2.01"

    (check-equal? (zero) '())

    (test-suite "successor"
      (parameterize ((base 4))
        (check-equal? (successor (zero)) '(1))
        (check-equal? (successor '(2 1)) '(3 1))
        (check-equal? (successor '(3 1 1)) '(0 2 1))
        (check-equal? (successor '(3 3 3)) '(0 0 0 1))
        (check-equal? (successor '(3 3 3 1)) '(0 0 0 2))))

    (test-suite "predecessor"
      (parameterize ((base 4))
        (check-equal? (predecessor '(1)) '())
        (check-equal? (predecessor '(3 1)) '(2 1))
        (check-equal? (predecessor '(0 2 1)) '(3 1 1))
        (check-equal? (predecessor '(0 0 0 1)) '(3 3 3))
        (check-equal? (predecessor '(0 0 0 2)) '(3 3 3 1))))

    (test-suite "bignum->int"
      (parameterize ((base 10))
        (check-equal? (bignum->int '()) 0)
        (check-equal? (bignum->int '(0 0 1)) 100)
        (check-equal? (bignum->int '(3 2 1)) 123)))

    (test-suite "int->bignum"
      (parameterize ((base 10))
        (check-equal? (int->bignum 0) '())
        (check-equal? (int->bignum 100) '(0 0 1))
        (check-equal? (int->bignum 321) '(1 2 3))))

    (test-suite "multiply"
      (parameterize ((base 10))
        (check-equal? (multiply (zero) '(6 2 3)) (zero))
        (check-equal? (bignum->int (multiply '(8 5 7) '(6 2 3)))
                      (* 758 326))))

    (test-suite "factorial"
      (check-equal? (factorial (int->bignum 5))
                    (int->bignum 120)))
))

(exit (run-tests eopl-2.01-tests))
