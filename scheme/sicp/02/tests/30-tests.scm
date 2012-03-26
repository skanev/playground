(require rackunit rackunit/text-ui)
(load "../30.scm")

(define sicp-2.30-tests
  (test-suite
    "Tests for SICP exercise 2.30"

    (check-equal?
      (square-tree (list 1
                         (list 2 (list 3 4) 5)
                         (list 6 7)))
      '(1 (4 (9 16) 25) (36 49)))
))

(run-tests sicp-2.30-tests)
