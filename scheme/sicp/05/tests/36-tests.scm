(require rackunit rackunit/text-ui)
(load "helpers/compiler.scm")
(load "../36.scm")

(define (run exp)
  (run-compiler-with-text explicit+compile-text extra-operations exp))

(define sicp-5.36-tests
  (test-suite
    "Tests for SICP exercise 5.36"

    (check-equal? (run '(begin (define order 'unknown)
                               (define (f a b) 'done)
                               (f (set! order 'right-to-left)
                                  (set! order 'left-to-right))
                               order))
                  'left-to-right)

    (check-equal? (run '(+ 1)) 1)
    (check-equal? (run '(- 2 1)) 1)
))

(run-tests sicp-5.36-tests)
