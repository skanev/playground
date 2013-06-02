(require rackunit rackunit/text-ui)
(load-relative "all.scm")
(load-relative "test-helpers.scm")

(define nameless-interpreter-tests
  (test-suite
    "Tests for the nameless interpreter"

    (check-equal? (run "42") 42)

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
                  -100)
))

(exit (run-tests nameless-interpreter-tests))
