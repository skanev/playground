(require rackunit rackunit/text-ui)
(load "../29.scm")

(define sicp-2.29-tests
  (test-suite
    "Tests for SICP exercise 2.29"

    (check-equal? (left-branch (make-mobile (make-branch 1 2)
                                            (make-branch 3 4)))
                  (make-branch 1 2))

    (check-equal? (right-branch (make-mobile (make-branch 1 2)
                                             (make-branch 3 4)))
                  (make-branch 3 4))

    (check-equal? (branch-length (make-branch 1 2))
                  1)

    (check-equal? (branch-structure (make-branch 1 2))
                  2)

    (check-equal?
      (total-weight (make-mobile (make-branch 1 (make-mobile (make-branch 2 3)
                                                             (make-branch 4 5)))
                                 (make-branch 6 7)))
      15)

    (check-true
      (balanced? (make-mobile (make-branch 3 4)
                              (make-branch 6 2))))

    (check-false
      (balanced? (make-mobile (make-branch 3 3)
                              (make-branch 6 2))))

    (check-true
      (balanced? (make-mobile (make-branch 3 (make-mobile (make-branch 2 4)
                                                          (make-branch 8 1)))
                              (make-branch 5 3))))

    (check-false
      (balanced? (make-mobile (make-branch 3 (make-mobile (make-branch 2 4)
                                                          (make-branch 7 1)))
                              (make-branch 5 3))))

    (check-false
      (balanced? (make-mobile (make-branch 3 (make-mobile (make-branch 2 4)
                                                          (make-branch 8 1)))
                              (make-branch 4 3))))
))

(run-tests sicp-2.29-tests)
