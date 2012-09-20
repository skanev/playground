(require rackunit rackunit/text-ui)
(load "../01.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.01-tests
  (test-suite
    "Tests for SICP exercise 4.01"

    (test-case "Left-to-right evaluation"
      (set-evaluation-order! 'left-to-right)
      (check-equal? (run '(begin (define x 1)
                                 (cons (set! x 2)
                                       (set! x 3))
                                 x))
                    3))

    (test-case "Right-to-left evaluation"
      (set-evaluation-order! 'right-to-left)
      (check-equal? (run '(begin (define x 1)
                                 (cons (set! x 2)
                                       (set! x 3))
                                 x))
                    2))
))

(run-tests sicp-4.01-tests)
