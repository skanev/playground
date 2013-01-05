(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../32.scm")

(define eopl-1.32-tests
  (test-suite
    "Tests for EOPL exercise 1.32"

    (check-equal?
      (double-tree
        (interior-node 'root
                       (interior-node 'left (leaf 1) (leaf 2))
                       (interior-node 'right
                                      (leaf 3)
                                      (interior-node 'right2 (leaf 4) (leaf 5)))))
      (interior-node 'root
                     (interior-node 'left (leaf 2) (leaf 4))
                     (interior-node 'right
                                    (leaf 6)
                                    (interior-node 'right2 (leaf 8) (leaf 10)))))
))

(exit (run-tests eopl-1.32-tests))
