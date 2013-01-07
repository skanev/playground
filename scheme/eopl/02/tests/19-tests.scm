(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../19.scm")

(define eopl-2.19-tests
  (test-suite
    "Tests for EOPL exercise 2.19"

    (check-equal? (number->bintree 13)
                  '(13 () ()))

    (check-equal? (insert-to-left 4 '(1 (2 () ()) (3 () ())))
                  '(1 (4 (2 () ()) ()) (3 () ())))

    (check-equal? (insert-to-right 4 '(1 (2 () ()) (3 () ())))
                  '(1 (2 () ()) (4 () (3 () ()))))

    (let ((t1 (insert-to-right 14 (insert-to-left 12 (number->bintree 13)))))
      (check-equal? t1 '(13 (12 () ()) (14 () ())))
      (check-equal? (move-to-left-son t1)
                    '(12 () ()))
      (check-equal? (current-element (move-to-left-son t1))
                    12)
      (check-equal? (at-leaf? (move-to-right-son (move-to-left-son t1)))
                    #t)
      (check-equal? (insert-to-left 15 t1)
                    '(13 (15 (12 () ()) ()) (14 () ()))))
))

(exit (run-tests eopl-2.19-tests))
