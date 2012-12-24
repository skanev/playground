(require rackunit rackunit/text-ui)
(load "helpers/memory.scm")
(load "../21.scm")

(define sicp-5.21-tests
  (test-suite
    "Tests for SICP exercise 5.21"

    (test-case "recursive count-leaves"
      (set-register-contents! count-leaves-machine 'tree
                              (allocate-list count-leaves-machine '((1 (2 3) (4 5)) ((6) 7))))
      (start count-leaves-machine)

      (check-equal? (get-register-contents count-leaves-machine 'result)
                    '(n 7)))

    (test-case "count-leaves with explicit counter"
      (set-register-contents! count-leaves-explicit-counter-machine 'tree
                              (allocate-list count-leaves-explicit-counter-machine
                                             '((1 (2 3) (4 5)) ((6) 7))))
      (start count-leaves-explicit-counter-machine)

      (check-equal? (get-register-contents count-leaves-explicit-counter-machine 'result)
                    '(n 7)))
))

(run-tests sicp-5.21-tests)
