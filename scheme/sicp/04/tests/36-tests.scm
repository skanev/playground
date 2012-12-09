(require rackunit rackunit/text-ui)
(load "../36.scm")

(define (first-n-values n exp)
  (define (take n results)
    (if (= n 0)
        '()
        (cons (car results)
              (take (- n 1) (force (cdr results))))))
  (take n
        (ambeval exp
                 solution-environment
                 (lambda (value fail) (cons value (delay (fail))))
                 (lambda () '()))))

(define sicp-4.36-tests
  (test-suite
    "Tests for SICP exercise 4.36"

    (check-equal? (first-n-values 5 '(a-pythagorean-triple))
                  '((3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17)))

))

(run-tests sicp-4.36-tests)
