(require rackunit rackunit/text-ui)
(load "../93.scm")

(define (poly var . coeffs)
  (define (to-term-list coeffs)
    (cond ((null? coeffs) '())
          ((=zero? (car coeffs)) (to-term-list (cdr coeffs)))
          (else (cons (list (- (length coeffs) 1) (car coeffs))
                      (to-term-list (cdr coeffs))))))

  (make-polynomial var (to-term-list coeffs)))

(define sicp-2.93-tests
  (test-suite
    "Tests for SICP exercise 2.93"

    (test-suite "rationals"
      (check-equal? (add (make-rational 1 2) (make-rational 3 4)) (make-rational 10 8))
      (check-equal? (sub (make-rational 3 4) (make-rational 1 2)) (make-rational 2 8))
      (check-equal? (mul (make-rational 2 3) (make-rational 4 5)) (make-rational 8 15))
      (check-equal? (div (make-rational 2 3) (make-rational 4 5)) (make-rational 10 12))
    )

    (test-suite "rational functions"
      (check-equal? (add (make-rational (poly 'x 1 0 1)
                                        (poly 'x 1 0 0 1))
                         (make-rational (poly 'x 1 0 1)
                                        (poly 'x 1 0 0 1)))
                    (make-rational (poly 'x 2 0 2 2 0 2)
                                   (poly 'x 1 0 0 2 0 0 1)))
    )
))

(run-tests sicp-2.93-tests)
