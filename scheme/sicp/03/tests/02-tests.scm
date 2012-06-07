(require rackunit rackunit/text-ui)
(load "../02.scm")

(define sicp-3.02-tests
  (test-suite
    "Tests for SICP exercise 3.02"

    (test-begin
      (let ((s (make-monitored sqrt)))
        (check-equal? (s 100) 10)
        (check-equal? (s 'how-many-calls?) 1)))

    (test-begin
      (let ((s (make-monitored sqrt)))
        (s 100)
        (s 100)
        (s 100)
        (check-equal? (s 'how-many-calls?) 3)))

    (test-begin
      (let ((s (make-monitored sqrt)))
        (s 100)
        (s 'reset-count)
        (check-equal? (s 'how-many-calls?) 0)))
))

(run-tests sicp-3.02-tests)
