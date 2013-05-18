(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../26.scm")
(load-relative "helpers/proc.scm")

(define (free-vars code)
  (free-variables (scan&parse code) '()))

(define eopl-3.26-tests
  (test-suite
    "Tests for EOPL exercise 3.26"

    (test-suite "Free-variables"
      (check-equal? (free-vars "zero?(a)") '(a))
      (check-equal? (free-vars "if a then b else c") '(a b c))
      (check-equal? (free-vars "let a = b in -(a, c)") '(b c))
      (check-equal? (free-vars "proc (x) -(a, x)") '(a))
      (check-equal? (free-vars "(a b)") '(a b))
      (check-equal? (free-vars "proc (x) proc (y) (y x)") '()))

    (test-suite "Simplifying environments"
      (check-equal? (slice-env '(b c) '((a 1) (b 2) (c 3) (d 4)))
                    '((b 2) (c 3)))
      (check-equal? (slice-env '(c b) '((a 1) (b 2) (c 3) (d 4)))
                    '((b 2) (c 3))))

    (test-suite "The evaluator"
      (check-equal? (run "42") 42)
      (check-equal? (eval* "x" (env '(x) '(10))) 10)
      (check-equal? (eval* "-(x, 7)" (env '(x) '(10))) 3)

      (check-equal? (run "% Comment\n 1") 1)

      (check-equal? (run "zero?(0)") #t)
      (check-equal? (run "zero?(1)") #f)

      (check-equal? (run "if zero?(0) then 1 else 2") 1)
      (check-equal? (run "if zero?(3) then 1 else 2") 2)

      (check-equal? (run "let x = 1 in x") 1)
      (check-equal? (run "let x = 1 in let x = 2 in x") 2)
      (check-equal? (run "let x = 1 in let y = 2 in x") 1)

      (check-equal? (run "let x = 7                  % This is a comment
                          in let y = 2               % This is another comment
                             in let y = let x = -(x, 1) in -(x, y)
                                in -(-(x, 8),y)")
                    -5)

      (check-equal? (run "let f = proc (x) -(x, 11)
                          in (f (f 77))")
                    55)

      (check-equal? (run "(proc (f) (f (f 77))
                           proc (x) -(x, 11))")
                    55)

      (check-equal? (run "let x = 200
                          in let f = proc (z) -(z, x)
                             in let x = 100
                                in let g = proc (z) -(z, x)
                                   in -((f 1), (g 1))")
                    -100))
))

(exit (run-tests eopl-3.26-tests))
