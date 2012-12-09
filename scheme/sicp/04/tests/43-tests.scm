(require rackunit rackunit/text-ui)
(load "../43.scm")

(define (all-values exp)
  (ambeval exp
           solution-environment
           (lambda (value fail) (cons value (fail)))
           (lambda () '())))

(define sicp-4.43-tests
  (test-suite
    "Tests for SICP exercise 4.43"

    (check-equal? (all-values '(yachts-and-daughters true))
                  '(downing))
    (check-equal? (all-values '(yachts-and-daughters false))
                  '(parker downing))
))

(run-tests sicp-4.43-tests)
