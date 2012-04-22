(require rackunit rackunit/text-ui)
(load "../68.scm")

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sicp-2.68-tests
  (test-suite
    "Tests for SICP exercise 2.68"

    (check-equal? (encode '(A D A B B C A) sample-tree)
                  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

    (check-exn exn? (lambda () (encode '(E) sample-tree)))
))

(run-tests sicp-2.68-tests)
