(require rackunit rackunit/text-ui)
(load "../44.scm")

(define (below a b) (list 'below a b))
(define (beside a b) (list 'beside a b))

(define sicp-2.44-tests
  (test-suite
    "Tests for SICP exercise 2.44"

    (check-equal? (up-split 'a 1)
                  '(below a (beside a a)))

    (check-equal? (up-split 'a 2)
                  '(below a
                          (beside (below a
                                         (beside a a))
                                  (below a
                                         (beside a a)))))


))

(run-tests sicp-2.44-tests)
