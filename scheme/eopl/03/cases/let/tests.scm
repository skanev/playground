(require rackunit rackunit/text-ui)
(load-relative "all.scm")
(load-relative "test-helpers.scm")

(define let-language-tests
  (test-suite
    "Tests for the LET language"

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
))

(exit (run-tests let-language-tests))
