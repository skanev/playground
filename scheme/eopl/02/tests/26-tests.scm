(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../26.scm")

(define eopl-2.26-tests
  (test-suite
    "Tests for EOPL exercise 2.26"

    (check-equal?
      (mark-with-red-depth
        (red-node
          (blue-node (list (red-node (leaf-node 0)
                                     (leaf-node 0))
                           (leaf-node 0)
                           (blue-node (list (leaf-node 0)
                                            (red-node (leaf-node 0)
                                                      (leaf-node 0))))))
          (leaf-node 0)))
      (red-node
        (blue-node (list (red-node (leaf-node 2)
                                   (leaf-node 2))
                         (leaf-node 1)
                         (blue-node (list (leaf-node 1)
                                          (red-node (leaf-node 2)
                                                    (leaf-node 2))))))
        (leaf-node 1)))
))

(exit (run-tests eopl-2.26-tests))
