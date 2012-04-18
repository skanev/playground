(require rackunit rackunit/text-ui)

(define (below a b) (list 'below a b))
(define (beside a b) (list 'beside a b))

(load "../45.scm")

(define sicp-2.45-tests
  (test-suite
    "Tests for SICP exercise 2.45"

    (check-equal? (up-split 'a 1)
                  '(below a (beside a a)))

    (check-equal? (up-split 'a 2)
                  '(below a
                          (beside (below a
                                         (beside a a))
                                  (below a
                                         (beside a a)))))

    (check-equal? (right-split 'a 1)
                  '(beside a (below a a)))

    (check-equal? (right-split 'a 2)
                  '(beside a 
                           (below (beside a
                                          (below a a))
                                  (beside a
                                          (below a a)))))
))

(run-tests sicp-2.45-tests)
