(require rackunit rackunit/text-ui)
(load "../04.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.04-tests
  (test-suite
    "Tests for SICP exercise 4.04"

    (test-case "And with evaluation procedures"
      (set-logical-operations-implementation! 'syntax-procedures)
      (check-equal? (run '(and)) true)
      (check-equal? (run '(and false)) false)
      (check-equal? (run '(and false false)) false)
      (check-equal? (run '(and 1)) 1)
      (check-equal? (run '(and 1 2)) 2)
      (check-equal? (run '(and false (fail))) false)
      (check-equal? (run '(begin (define (x) 2)
                                 (and 1 (x))))
                    2))

    (test-case "And as a derived form"
      (set-logical-operations-implementation! 'derived-forms)
      (check-equal? (run '(and)) true)
      (check-equal? (run '(and false)) false)
      (check-equal? (run '(and false false)) false)
      (check-equal? (run '(and 1)) 1)
      (check-equal? (run '(and 1 2)) 2)
      (check-equal? (run '(and false (fail))) false)
      (check-equal? (run '(begin (define (x) 2)
                                 (and 1 (x))))
                    2))

    (test-case "Or with evaluation procedures"
      (set-logical-operations-implementation! 'syntax-procedures)
      (check-equal? (run '(or)) false)
      (check-equal? (run '(or false)) false)
      (check-equal? (run '(or false false)) false)
      (check-equal? (run '(or 1)) 1)
      (check-equal? (run '(or 1 2)) 1)
      (check-equal? (run '(or 1 (fail))) 1)
      (check-equal? (run '(or false 2)) 2)
      (check-equal? (run '(begin (define (x) false)
                                 (or (x) 1)))
                    1))

    (test-case "Or as a derived form"
      (set-logical-operations-implementation! 'derived-forms)
      (check-equal? (run '(or)) false)
      (check-equal? (run '(or false)) false)
      (check-equal? (run '(or false false)) false)
      (check-equal? (run '(or 1)) true)
      (check-equal? (run '(or 1 2)) true)
      (check-equal? (run '(or 1 (fail))) true)
      (check-equal? (run '(or false 2)) true)
      (check-equal? (run '(begin (define (x) false)
                                 (or (x) 1)))
                    true))
))

(run-tests sicp-4.04-tests)
