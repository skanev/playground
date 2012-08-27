(require rackunit rackunit/text-ui)
(load "../96.scm")

(define (poly var . coeffs)
  (define (to-term-list coeffs)
    (cond ((null? coeffs) '())
          ((=zero? (car coeffs)) (to-term-list (cdr coeffs)))
          (else (cons (list (- (length coeffs) 1) (car coeffs))
                      (to-term-list (cdr coeffs))))))

  (make-polynomial var (to-term-list coeffs)))

(define p1 (poly 'x 1 -2 1))
(define p2 (poly 'x 11 0 7))
(define p3 (poly 'x 13 5))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(define sicp-2.96-tests
  (test-suite
    "Tests for SICP exercise 2.96"

    (check-equal? (greatest-common-divisor q1 q2) p1)
))

(run-tests sicp-2.96-tests)
