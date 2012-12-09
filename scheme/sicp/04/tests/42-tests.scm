(require rackunit rackunit/text-ui)
(load "../42.scm")

(define (all-values exp)
  (ambeval exp
           solution-environment
           (lambda (value fail) (cons value (fail)))
           (lambda () '())))

(define sicp-4.42-tests
  (test-suite
    "Tests for SICP exercise 4.42"

    (check-equal? (all-values '(lairs))
                  '(((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))))

))

(run-tests sicp-4.42-tests)
