(require rackunit rackunit/text-ui)
(load "../08.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.08-tests
  (test-suite
    "Tests for SICP exercise 4.08"


    (check-equal? (run '(let ((x 1)) x)) 1)
    (check-equal? (run '(let ((a 1) (b 2)) (+ a b))) 3)
    (check-equal? (run '(let ((a 1) (b 2)) a b)) 2)
    (check-equal? (run '(begin (define a 1)
                               (let ((b 2)
                                     (c 3))
                                 (+ a b c))))
                  6)

    (check-equal? (run '(begin (define (fib n)
                                 (let fib-iter ((a 1)
                                                (b 0)
                                                (count n))
                                   (if (= count 0)
                                     b
                                     (fib-iter (+ a b) a (- count 1)))))
                               (fib 12)))
                  144)
))

(run-tests sicp-4.08-tests)
