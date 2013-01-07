(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../24.scm")

(define eopl-2.24-tests
  (test-suite
    "Tests for EOPL exercise 2.24"

    (check-equal? (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
                  '(interior-node a (leaf-node 3) (leaf-node 4)))
))

(exit (run-tests eopl-2.24-tests))
