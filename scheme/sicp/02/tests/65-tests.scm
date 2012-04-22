(require rackunit rackunit/text-ui)
(load "../65.scm")

(define sicp-2.65-tests
  (test-suite
    "Tests for SICP exercise 2.65"

    (check-equal?
      (tree->list (intersection-set (list->tree '(1 3 5 7 9 11))
                                    (list->tree '(2 3 5 9 10))))
      '(3 5 9))

    (check-equal?
      (tree->list (union-set (list->tree '(1 3 5 7 9 11))
                             (list->tree '(2 3 5 9 10))))
      '(1 2 3 5 7 9 10 11))
))

(run-tests sicp-2.65-tests)
