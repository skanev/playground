(require rackunit rackunit/text-ui)
(load "../48.scm")

(define (all-values exp)
  (ambeval exp
           solution-environment
           (lambda (value fail) (cons value (fail)))
           (lambda () '())))

(define (parses? exp)
  (not (null? (all-values `(parse (quote ,exp))))))

(define sicp-4.48-tests
  (test-suite
    "Tests for SICP exercise 4.48"

    (check-true (parses? '(the cat sleeps quietly)))
    (check-true (parses? '(the brown cat sleeps)))
    (check-true (parses? '(the quick brown cat sleeps)))
    (check-true (parses? '(the quick brown cat sleeps in the class)))
    (check-true (parses? '(the quick brown cat sleeps in the class quietly)))
))

(run-tests sicp-4.48-tests)
