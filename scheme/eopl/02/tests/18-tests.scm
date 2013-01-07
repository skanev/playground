(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../18.scm")

(define eopl-2.18-tests
  (test-suite
    "Tests for EOPL exercise 2.18"

    (check-equal? (number->sequence 7)
                  '(7 () ()))
    (check-equal? (current-element '(6 (5 4 3 2 1) (7 8 9)))
                  '6)
    (check-equal? (move-to-left '(6 (5 4 3 2 1) (7 8 9)))
                  '(5 (4 3 2 1) (6 7 8 9)))
    (check-equal? (move-to-right '(6 (5 4 3 2 1) (7 8 9)))
                  '(7 (6 5 4 3 2 1) (8 9)))
    (check-equal? (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
                  '(6 (13 5 4 3 2 1) (7 8 9)))
    (check-equal? (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
                  '(6 (5 4 3 2 1) (13 7 8 9)))

    (check-exn exn? (lambda () (move-to-right '(2 (1) ()))))
    (check-exn exn? (lambda () (move-to-left '(2 () (3)))))
    (check-true (at-left-end? '(2 () (3))))
    (check-false (at-left-end? '(2 (1) (3))))
    (check-true (at-right-end? '(2 (1) ())))
    (check-false (at-right-end? '(2 (1) (3))))
))

(exit (run-tests eopl-2.18-tests))
