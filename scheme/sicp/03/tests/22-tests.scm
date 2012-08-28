(require rackunit rackunit/text-ui)
(load "../22.scm")

(define sicp-3.22-tests
  (test-suite
    "Tests for SICP exercise 3.22"

    (test-begin "empty-queue?"
      (check-true (empty-queue? (make-queue))))

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
      (check-equal? 'b (front-queue q)))
))

(run-tests sicp-3.22-tests)
