(require rackunit rackunit/text-ui)
(load "../23.scm")

(define sicp-3.23-tests
  (test-suite
    "Tests for SICP exercise 3.23"

    (test-case "empty-deque?"
      (check-true (empty-deque? (make-deque))))

    (test-case "front-insert-deque!"
      (define deque (make-deque))
      (front-insert-deque! deque 'a)
      (front-insert-deque! deque 'b)
      (front-insert-deque! deque 'c)

      (check-equal? (front-deque deque) 'c)
      (check-equal? (rear-deque deque) 'a))

    (test-case "rear-insert-deque!"
      (define deque (make-deque))
      (rear-insert-deque! deque 'a)
      (rear-insert-deque! deque 'b)
      (rear-insert-deque! deque 'c)

      (check-equal? (front-deque deque) 'a)
      (check-equal? (rear-deque deque) 'c))

    (test-case "front-delete-deque!"
      (define deque (make-deque))
      (rear-insert-deque! deque 'a)
      (rear-insert-deque! deque 'b)
      (rear-insert-deque! deque 'c)

      (front-delete-deque! deque)

      (check-equal? (front-deque deque) 'b)
      (check-equal? (rear-deque deque) 'c)

      (front-delete-deque! deque)
      (check-equal? (front-deque deque) 'c)
      (check-equal? (rear-deque deque) 'c)

      (front-delete-deque! deque)
      (check-true (empty-deque? deque)))

    (test-case "rear-delete-deque!"
      (define deque (make-deque))
      (rear-insert-deque! deque 'a)
      (rear-insert-deque! deque 'b)
      (rear-insert-deque! deque 'c)

      (rear-delete-deque! deque)

      (check-equal? (front-deque deque) 'a)
      (check-equal? (rear-deque deque) 'b)

      (rear-delete-deque! deque)

      (check-equal? (front-deque deque) 'a)
      (check-equal? (rear-deque deque) 'a)

      (rear-delete-deque! deque)
      (check-true (empty-deque? deque)))
))

(run-tests sicp-3.23-tests)
