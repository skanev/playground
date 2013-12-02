(require rackunit rackunit/text-ui)
(load-relative "all.scm")
(load-relative "test-helpers.scm")

(define explicit-refs-language-tests
  (test-suite
    "Tests for the EXPLICIT-REFS language"

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
                  -100)

    (check-equal? (run "begin 1; 2 end")
                  2)

    (initialize-store!)
    (check-equal? (run "let g = let counter = newref(0)
                                in proc (dummy)
                                     begin
                                       setref(counter, -(deref(counter), -(0, 1)));
                                       deref(counter)
                                     end
                        in let a = (g 11)
                           in let b = (g 11)
                              in -(a, b)")
                  -1)
))

(exit (run-tests explicit-refs-language-tests))
