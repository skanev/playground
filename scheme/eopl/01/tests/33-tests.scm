(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../33.scm")

(define eopl-1.33-tests
  (test-suite
    "Tests for EOPL exercise 1.33"

    (check-equal?
      (mark-leaves-with-red-depth
        (interior-node 'red
          (interior-node 'bar
            (leaf 26)
            (leaf 12))
          (interior-node 'red
            (leaf 11)
            (interior-node 'quux
              (leaf 117)
              (leaf 14)))))
      '(red
         (bar 1 1)
         (red 2 (quux 2 2))))
))

(exit (run-tests eopl-1.33-tests))
