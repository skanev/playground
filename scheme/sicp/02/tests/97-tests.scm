(require rackunit rackunit/text-ui)
(load "../97.scm")

(define (poly var . coeffs)
  (define (to-term-list coeffs)
    (cond ((null? coeffs) '())
          ((=zero? (car coeffs)) (to-term-list (cdr coeffs)))
          (else (cons (list (- (length coeffs) 1) (car coeffs))
                      (to-term-list (cdr coeffs))))))

  (make-polynomial var (to-term-list coeffs)))

(define sicp-2.97-tests
  (test-suite
    "Tests for SICP exercise 2.97"

    (check-equal? (make-rational 8 10) (make-rational 4 5))
    (check-equal? (reduce (poly 'x 1 2 1) (poly 'x 1 0 -1))
                  (list (poly 'x 1 1) (poly 'x 1 -1)))
    (check-equal? (add (make-rational (poly 'x 1 1)
                                      (poly 'x 1 0 0 -1))
                       (make-rational (poly 'x 1 0)
                                      (poly 'x 1 0 -1)))
                  (make-rational (poly 'x -1 -2 -3 -1)
                                 (poly 'x -1 -1 0 1 1)))
))

(run-tests sicp-2.97-tests)
