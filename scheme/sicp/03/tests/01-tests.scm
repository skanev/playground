(require rackunit rackunit/text-ui)
(load "../01.scm")

(define sicp-3.01-tests
  (test-suite
    "Tests for SICP exercise 3.01"

    (check-equal? ((make-accumulator 10) 5) 15)

    (test-begin
      (let ((accumulator (make-accumulator 0)))
        (accumulator 10)
        (accumulator 20)
        (check-equal? (accumulator 30) 60)))

    (test-begin
      (let ((first (make-accumulator 0))
            (second (make-accumulator 0)))
        (check-equal? (first 10) 10)
        (check-equal? (second 20) 20)
        (check-equal? (first 10) 20)
        (check-equal? (second 20) 40)))


))

(run-tests sicp-3.01-tests)
