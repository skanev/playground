(require rackunit rackunit/text-ui)
(load "../67.scm")

(define sicp-2.67-tests
  (test-suite
    "Tests for SICP exercise 2.67"

    (check-equal? (decode sample-message sample-tree)
                  '(A D A B B C A))
))

(run-tests sicp-2.67-tests)
