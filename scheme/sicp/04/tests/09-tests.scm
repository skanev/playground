(require rackunit rackunit/text-ui)
(load "../09.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.09-tests
  (test-suite
    "Tests for SICP exercise 4.09"

    (test-suite "For"
      (check-equal? (run '(begin (define sum 0)
                                 (for x '(1 2 3 4 5)
                                      (set! sum (+ sum x)))
                                 sum))
                    15)
      (check-equal? (run '(begin (define sum 0)
                                 (for x '(1 2 3 4 5)
                                      (set! sum (+ sum x))
                                      (set! sum (+ sum x)))
                                 sum))
                    30))

    (test-suite "While"
      (check-equal? (run '(begin (define sum 0)
                                 (define n 1)
                                 (while (< n 10)
                                        (set! sum (+ sum n))
                                        (set! n (+ n 1)))
                                 sum))
                    45))

    (test-suite "Until"
      (check-equal? (run '(begin (define sum 0)
                                 (define n 1)
                                 (until (= n 10)
                                        (set! sum (+ sum n))
                                        (set! n (+ n 1)))
                                 sum))
                    45))
))

(run-tests sicp-4.09-tests)
