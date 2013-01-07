(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../25.scm")

(define eopl-2.25-tests
  (test-suite
    "Tests for EOPL exercise 2.25"

    (let* ((tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3)))
           (tree-2 (interior-node 'bar (leaf-node -1) tree-1))
           (tree-3 (interior-node 'baz tree-2 (leaf-node 1))))
      (check-equal? 'foo (max-interior tree-2))
      (check-equal? 'baz (max-interior tree-3)))
))

(exit (run-tests eopl-2.25-tests))
