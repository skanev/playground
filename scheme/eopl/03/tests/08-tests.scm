(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../08.scm")
(load-relative "helpers/let.scm")

(define eopl-3.08-tests
  (test-suite
    "Tests for EOPL exercise 3.08"

    (check-true (run "equal?(1, 1)"))
    (check-false (run "equal?(1, 2)"))

    (check-true (run "less?(1, 2)"))
    (check-false (run "less?(1, 1)"))
    (check-false (run "less?(1, 0)"))

    (check-true (run "greater?(1, 0)"))
    (check-false (run "greater?(1, 1)"))
    (check-false (run "greater?(1, 2)"))

    (check-equal? (run "+(4, 5)") 9)
    (check-equal? (run "*(7, 4)") 28)
    (check-equal? (run "/(10, 3)") 3)

    (check-equal? (run "minus(-(minus(5), 9))") 14)

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

(exit (run-tests eopl-3.08-tests))
