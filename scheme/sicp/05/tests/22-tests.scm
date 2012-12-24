(require rackunit rackunit/text-ui)
(load "helpers/memory.scm")
(load "../22.scm")

(define sicp-5.22-tests
  (test-suite
    "Tests for SICP exercise 5.22"

    (test-suite "append"
      (set-register-contents! append-machine 'x (allocate-list append-machine '(1 2 3)))
      (set-register-contents! append-machine 'y (allocate-list append-machine '(4 5 6)))

      (check-equal? (memory-dump append-machine)
                    '((n1 n2 n3 n4 n5 n6 __ __ __ __ __ __ __ __ __ __ __ __ __ __)
                      (p1 p2 e0 p4 p5 e0 __ __ __ __ __ __ __ __ __ __ __ __ __ __)))

      (check-equal? (list-in-memory append-machine (get-register-contents append-machine 'x))
                    '(1 2 3))
      (check-equal? (list-in-memory append-machine (get-register-contents append-machine 'y))
                    '(4 5 6))

      (start append-machine)

      (check-equal? (list-in-memory append-machine
                                    (get-register-contents append-machine 'result))
                    '(1 2 3 4 5 6))

      (check-equal? (memory-dump append-machine)
                    '((n1 n2 n3 n4 n5 n6 n1 n2 n3 __ __ __ __ __ __ __ __ __ __ __)
                      (p1 p2 e0 p4 p5 e0 p7 p8 p3 __ __ __ __ __ __ __ __ __ __ __))))

    (test-suite "append!"
      (set-register-contents! append!-machine 'x (allocate-list append!-machine '(1 2 3)))
      (set-register-contents! append!-machine 'y (allocate-list append!-machine '(4 5 6)))

      (check-equal? (memory-dump append!-machine)
                    '((n1 n2 n3 n4 n5 n6 __ __ __ __ __ __ __ __ __ __ __ __ __ __)
                      (p1 p2 e0 p4 p5 e0 __ __ __ __ __ __ __ __ __ __ __ __ __ __)))

      (start append!-machine)

      (check-equal? (memory-dump append!-machine)
                    '((n1 n2 n3 n4 n5 n6 __ __ __ __ __ __ __ __ __ __ __ __ __ __)
                      (p1 p2 p3 p4 p5 e0 __ __ __ __ __ __ __ __ __ __ __ __ __ __))))
))

(run-tests sicp-5.22-tests)
