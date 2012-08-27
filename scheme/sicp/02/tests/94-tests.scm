(require rackunit rackunit/text-ui)
(load "../94.scm")

(define (poly var . coeffs)
  (define (to-term-list coeffs)
    (cond ((null? coeffs) '())
          ((=zero? (car coeffs)) (to-term-list (cdr coeffs)))
          (else (cons (list (- (length coeffs) 1) (car coeffs))
                      (to-term-list (cdr coeffs))))))

  (make-polynomial var (to-term-list coeffs)))

(define sicp-2.94-tests
  (test-suite
    "Tests for SICP exercise 2.94"

    (check-equal? (greatest-common-divisor 10 6) 2)
    (check-equal? (greatest-common-divisor (poly 'x 1 -1 -2 2)
                                           (poly 'x 1 0 -1))
                  (poly 'x -1 1))
    (check-equal? (greatest-common-divisor (poly 'x 1 0 -1)
                                           (poly 'x 1 -1 -2 2))
                  (poly 'x -1 1))
))

(run-tests sicp-2.94-tests)
