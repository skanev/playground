(require rackunit rackunit/text-ui)
(load "../21.scm")

(define sicp-3.21-tests
  (test-suite
    "Tests for SICP exercise 3.21"

    (test-begin "print-queue"
      (define q (make-queue))

      (insert-queue! q 'a)
      (insert-queue! q 'b)
      (insert-queue! q 'c)

      (check-equal? (with-output-to-string (lambda () (print-queue q)))
                    "#<queue:(a b c)>\n"))

    (test-suite "queue operations"
      (test-begin "insert-queue!"
        (define q (make-queue))

        (insert-queue! q 'a)
        (check-equal? 'a (front-queue q)))

      (test-begin "delete-queue!"
        (define q (make-queue))

        (insert-queue! q 'a)
        (insert-queue! q 'b)

        (check-equal? 'a (front-queue q))

        (delete-queue! q)
        (check-equal? 'b (front-queue q))))
))

(run-tests sicp-3.21-tests)
