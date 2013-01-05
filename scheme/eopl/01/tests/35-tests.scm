(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../35.scm")

(define eopl-1.35-tests
  (test-suite
    "Tests for EOPL exercise 1.35"

    (check-equal?
      (number-leaves
        (interior-node 'foo
          (interior-node 'bar
            (leaf 26)
            (leaf 12))
          (interior-node 'baz
            (leaf 11)
            (interior-node 'quux
              (leaf 117)
              (leaf 14)))))
      '(foo
         (bar 0 1)
         (baz
           2
           (quux 3 4))))
))

(exit (run-tests eopl-1.35-tests))
