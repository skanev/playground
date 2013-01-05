(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../03.scm")

(define eopl-2.03-tests
  (test-suite
    "Tests for EOPL exercise 2.03"

    (check-equal? (zero) '(diff (one) (one)))

    (check-equal? (successor '(one))
                  '(diff (one) (diff (diff (one) (one))
                                     (one))))

    (check-equal? (predecessor '(one))
                  '(diff (one) (one)))
    (check-equal? (predecessor '(diff (one) (one)))
                  '(diff (diff (one) (one)) (one)))

    (check-equal? (diff-tree-plus '(diff (one) (one))
                                  '(one))
                  '(diff (diff (one) (one))
                         (diff (diff (one) (one)) (one))))

    (check-equal? (diff-tree-plus '(diff (one) (one))
                                  '(diff (diff (one) (one)) (one)))
                  '(diff (diff (one) (one))
                         (diff (one) (diff (one) (one)))))

    (check-true (is-zero? (zero)))
    (check-true (is-zero? '(diff (diff (one) (diff (one) (one)))
                                 (diff (one) (diff (one) (one))))))
    (check-true (is-zero? '(diff (diff (one) (diff (one) (one)))
                                 (one))))
    (check-false (is-zero? '(diff (one) (diff (one) (one)))))

    (check-true (is-zero? ((compose successor predecessor) (zero))))
    (check-true (is-zero? ((compose predecessor successor) (zero))))

    (check-true (is-zero? ((compose successor successor predecessor predecessor) (zero))))
    (check-true (is-zero? ((compose successor predecessor successor predecessor) (zero))))
    (check-true (is-zero? ((compose successor predecessor predecessor successor) (zero))))

    (check-true (is-zero? ((compose predecessor predecessor successor successor) (zero))))
    (check-true (is-zero? ((compose predecessor successor predecessor successor) (zero))))
    (check-true (is-zero? ((compose predecessor successor successor predecessor) (zero))))

    (check-false (is-zero? (predecessor (zero))))
    (check-false (is-zero? (successor (zero))))

    (check-false (is-zero? ((compose predecessor predecessor) (zero))))
    (check-false (is-zero? ((compose predecessor predecessor predecessor) (zero))))
    (check-false (is-zero? ((compose predecessor predecessor successor) (zero))))
    (check-false (is-zero? ((compose predecessor successor predecessor) (zero))))
    (check-false (is-zero? ((compose predecessor successor successor) (zero))))

    (check-false (is-zero? ((compose successor successor) (zero))))
    (check-false (is-zero? ((compose successor successor successor) (zero))))
    (check-false (is-zero? ((compose successor successor predecessor) (zero))))
    (check-false (is-zero? ((compose successor predecessor successor) (zero))))
    (check-false (is-zero? ((compose successor predecessor predecessor) (zero))))
))

(exit (run-tests eopl-2.03-tests))
