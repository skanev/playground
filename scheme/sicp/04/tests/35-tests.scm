(require rackunit rackunit/text-ui)
(load "../35.scm")

(define (all-values exp)
  (ambeval exp
           solution-environment
           (lambda (value fail) (cons value (fail)))
           (lambda () '())))

(define sicp-4.35-tests
  (test-suite
    "Tests for SICP exercise 4.35"

    (check-equal? (all-values '(begin (a-pythagorean-triple-between 2 10)))
                  '((3 4 5) (6 8 10)))
))

(run-tests sicp-4.35-tests)
