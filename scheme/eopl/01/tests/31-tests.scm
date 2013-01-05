(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../31.scm")

(define eopl-1.31-tests
  (test-suite
    "Tests for EOPL exercise 1.31"

    (check-equal? (leaf 10) 10)
    (check-exn exn? (lambda () (leaf 'symbol)))

    (check-true (leaf? 10))
    (check-false (leaf? '(a 1 2)))

    (check-equal? (interior-node 'a (leaf 1) (leaf 2)) '(a 1 2))
    (check-exn exn? (lambda () (interior-node 1 (leaf 1) (leaf 2))))

    (check-equal? (lson (interior-node 'node (leaf 1) (leaf 2)))
                  (leaf 1))
    (check-equal? (rson (interior-node 'node (leaf 1) (leaf 2)))
                  (leaf 2))

    (check-equal? (contents-of (interior-node 'node (leaf 1) (leaf 2)))
                  'node)
    (check-equal? (contents-of (leaf 1))
                  1)
))

(exit (run-tests eopl-1.31-tests))
