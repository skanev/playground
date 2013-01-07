(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../20.scm")

(define eopl-2.20-tests
  (test-suite
    "Tests for EOPL exercise 2.20"

    (check-equal? (move-to-left-son (insert-to-left 2 (number->bintree 1)))
                  '(2 () () (1 (2 () () ()) () ())))
    (check-equal? (move-to-right-son (insert-to-right 2 (number->bintree 1)))
                  '(2 () () (1 () (2 () () ()) ())))

    (check-equal? (move-up (move-to-left-son (insert-to-left 2 (number->bintree 1))))
                  (insert-to-left 2 (number->bintree 1)))
    (check-equal? (move-up (move-to-right-son (insert-to-right 2 (number->bintree 1))))
                  (insert-to-right 2 (number->bintree 1)))
    (check-true (at-leaf?
                  (move-to-left-son
                    (move-to-left-son
                      (insert-to-left 2 (number->bintree 1))))))
    (check-true (at-leaf?
                  (move-to-right-son
                    (move-to-right-son
                      (insert-to-right 2 (number->bintree 1))))))
    (check-equal?
      (move-up
        (move-up
          (move-to-left-son
            (move-to-left-son
              (insert-to-left 2 (number->bintree 1))))))
      (insert-to-left 2 (number->bintree 1)))

    (check-equal?
      (move-up
        (move-up
          (move-to-right-son
            (move-to-right-son
              (insert-to-right 2 (number->bintree 1))))))
      (insert-to-right 2 (number->bintree 1)))

    (check-true (at-root? (number->bintree 1)))
    (check-false (at-root? (move-to-right-son (insert-to-right 2 (number->bintree 1)))))
))

(exit (run-tests eopl-2.20-tests))
