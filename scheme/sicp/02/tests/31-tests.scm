(require rackunit rackunit/text-ui)
(load "../31.scm")

(define sicp-2.31-tests
  (test-suite
    "Tests for SICP exercise 2.31"

    (check-equal? (square-tree '(1 (2 (3 4) 5) (6 7)))
                  '(1 (4 (9 16) 25) (36 49)))

    (check-equal? (tree-map (lambda (x) (+ x 1))
                            '(1 (2 (3 4) 5) (6 7)))
                  '(2 (3 (4 5) 6) (7 8)))
))

(run-tests sicp-2.31-tests)
