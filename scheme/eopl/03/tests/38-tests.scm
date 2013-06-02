(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../38.scm")
(load-relative "helpers/nameless.scm")

(define eopl-3.38-tests
  (test-suite
    "Tests for EOPL exercise 3.38"

    (check-equal? (run "let zero = 0
                        in let one = 1
                           in let two = 2
                              in let three = 3
                                 in cond zero?(0) ==> one
                                         zero?(1) ==> two
                                         zero?(1) ==> three
                                    end")
                  1)
    (check-equal? (run "let zero = 0
                        in let one = 1
                           in let two = 2
                              in let three = 3
                                 in cond zero?(1) ==> one
                                         zero?(0) ==> two
                                         zero?(1) ==> three
                                    end")
                  2)
    (check-equal? (run "let zero = 0
                        in let one = 1
                           in let two = 2
                              in let three = 3
                                 in cond zero?(1) ==> one
                                         zero?(1) ==> two
                                         zero?(0) ==> three
                                    end")
                  3)
    (check-equal? (run "let zero = 0
                        in let one = 1
                           in let two = 2
                              in let three = 3
                                 in cond zero?(1) ==> one
                                         zero?(zero) ==> two
                                         zero?(1) ==> three
                                    end")
                  2)
    (check-equal? (run "let zero = 0
                        in let one = 1
                           in let two = 2
                              in let three = 3
                                 in cond zero?(1) ==> one
                                         zero?(1) ==> two
                                         zero?(1) ==> three
                                    end")
                  #f)

    (check-equal? (run "cond zero?(0) ==> 0
                             zero?(1) ==> 1
                             zero?(1) ==> 2
                             end")
                  0)
    (check-equal? (run "cond zero?(1) ==> 0
                             zero?(0) ==> 1
                             zero?(1) ==> 2
                             end")
                  1)
    (check-equal? (run "cond zero?(1) ==> 0
                             zero?(1) ==> 1
                             zero?(0) ==> 2
                             end")
                  2)
    (check-equal? (run "cond zero?(1) ==> 0
                             zero?(1) ==> 1
                             zero?(1) ==> 2
                             end")
                  #f)

    (check-equal? (run "cond zero?(1) ==> 0
                             zero?(1) ==> 1
                             zero?(1) ==> 2
                             end")
                  #f)


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

(exit (run-tests eopl-3.38-tests))
