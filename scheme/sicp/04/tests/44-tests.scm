(require rackunit rackunit/text-ui)
(load "../44.scm")

(define (all-values exp)
  (ambeval exp
           solution-environment
           (lambda (value fail) (cons value (fail)))
           (lambda () '())))

(define sicp-4.44-tests
  (test-suite
    "Tests for SICP exercise 4.44"

    (check-equal? (length (all-values '(queens 8)))
                  92)
    (check member '(4 2 8 5 7 1 3 6) (all-values '(queens 8)))
))

(run-tests sicp-4.44-tests)
