(require rackunit rackunit/text-ui)
(load "../90.scm")

(define (t order n)
  (list order (make-integer n)))

(define (sparse . terms)
  (cons 'sparse terms))

(define (dense . coeffs)
  (cons 'dense (map make-integer coeffs)))

(define (poly var term-list)
  (make-polynomial var term-list))

(define sicp-2.90-tests
  (test-suite
    "Tests for SICP exercise 2.90"

    (test-suite "sparse representation"
      (check-true (empty-termlist? (the-empty-sparse-termlist)))
      (check-equal? (first-term (sparse (t 2 1))) (t 2 1))
      (check-equal? (rest-terms (sparse (t 2 1) (t 1 2)))
                    (sparse (t 1 2)))
      (check-equal? (adjoin-term (t 2 2) (sparse (t 1 1)))
                    (sparse (t 2 2) (t 1 1)))
      (check-equal? (adjoin-term (t 2 0) (sparse (t 1 1)))
                    (sparse (t 1 1)))
      (check-equal? (adjoin-term (t 1 -1) (sparse (t 2 1) (t 1 1) (t 0 1)))
                    (sparse (t 2 1) (t 0 1)))
    )

    (test-suite "dense representation"
      (check-true (empty-termlist? (the-empty-dense-termlist)))
      (check-equal? (first-term (dense 10 20 30)) (t 2 10))
      (check-equal? (rest-terms (dense 10 20 30)) (dense 20 30))
      (check-equal? (adjoin-term (t 2 1) (dense 1))
                    (dense 1 0 1))
      (check-equal? (adjoin-term (t 0 1) (dense 1 1))
                    (dense 1 2))
      (check-equal? (adjoin-term (t 0 1) (dense 1 0 0))
                    (dense 1 0 1))
      (check-equal? (adjoin-term (t 2 0) (dense 1))
                    (dense 1))
    )

    (test-suite "polynomial operations"
      (check-equal? (add (poly 'x (sparse (t 2 1)))
                         (poly 'x (sparse (t 1 0))))
                    (poly 'x (sparse (t 2 1) (t 1 0))))
      (check-equal? (mul (poly 'x (sparse (t 1 1) (t 0 1)))
                         (poly 'x (sparse (t 1 1) (t 0 -1))))
                    (poly 'x (sparse (t 2 1) (t 0 -1))))
      (check-equal? (sub (poly 'x (sparse (t 2 3) (t 1 3) (t 0 3)))
                         (poly 'x (sparse (t 2 3) (t 1 2) (t 0 1))))
                    (poly 'x (sparse (t 1 1) (t 0 2))))

      (check-equal? (add (poly 'x (dense 2 0))
                         (poly 'x (dense 1)))
                    (poly 'x (dense 2 1)))
      (check-equal? (mul (poly 'x (dense 1 1))
                         (poly 'x (dense 1 -1)))
                    (poly 'x (dense 1 0 -1)))
    )
))

(run-tests sicp-2.90-tests)
