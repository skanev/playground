(require rackunit rackunit/text-ui)
(load "../69.scm")

(define sicp-2.69-tests
  (test-suite
    "Tests for SICP exercise 2.69"

    (check-equal?
      (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
      (make-code-tree (make-leaf 'A 4)
                      (make-code-tree
                        (make-leaf 'B 2)
                        (make-code-tree (make-leaf 'D 1)
                                        (make-leaf 'C 1)))))
))

(run-tests sicp-2.69-tests)
