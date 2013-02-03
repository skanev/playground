(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../17.scm")

(define (run code)
  (eval* code (empty-env)))

(define (expval->schemeval val)
  (cases expval val
    (num-val (num) num)
    (bool-val (bool) bool)
    (emptylist-val () '())
    (pair-val (car cdr)
      (cons (expval->schemeval car)
            (expval->schemeval cdr)))))

(define (schemeval->expval val)
  (cond ((null? val) (emptylist-val))
        ((pair? val) (pair-val (schemeval->expval (car val)) (schemeval->expval (cdr val))))
        ((number? val) (num-val val))
        ((boolean? val) (bool-val val))
        (else (error 'schemeval->expval "Don't know how to convert ~s" val))))

(define (eval* code env)
  (let* ((expr (scan&parse code))
         (result (value-of expr env)))
    (expval->schemeval result)))

(define (env vars vals)
  (extend-env* vars
               (map schemeval->expval vals)
               (empty-env)))

(define eopl-3.17-tests
  (test-suite
    "Tests for EOPL exercise 3.17"

    (check-equal? (run "let x = 30
                            in let* x = -(x, 1) y = -(x, 2)
                                    in -(x, y)")
                  2)

    (check-equal? (run "let x = 10
                            y = 20
                            in +(x, y)")
                  30)

    (check-equal? (with-output-to-string (lambda () (run "print 42")))
                  "42")
    (check-equal? (with-output-to-string (lambda () (run "print zero?(1)")))
                  "#f")
    (check-equal? (with-output-to-string (lambda () (run "print emptylist")))
                  "emptylist")
    (check-equal? (with-output-to-string (lambda () (run "print cons(1, emptylist)")))
                  "cons(1, emptylist)")

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

    (check-equal? (run "list(1)") '(1))
    (check-equal? (run "list(1, 2, 3)") '(1 2 3))
    (check-equal? (run "let x = 4
                        in list(x, -(x, 1), -(x, 3))")
                  '(4 3 1))

    (check-equal? (run "emptylist") '())
    (check-equal? (run "cons(1, 2)") '(1 . 2))
    (check-equal? (run "car(cons(1, 2))") 1)
    (check-equal? (run "cdr(cons(1, 2))") 2)

    (check-true  (run "null?(emptylist)"))
    (check-false (run "null?(cons(1, 2))"))

    (check-equal? (run "let x = 4
                        in cons(x,
                                cons(cons(-(x, 1),
                                          emptylist),
                                     emptylist))")
                  '(4 (3)))

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

(exit (run-tests eopl-3.17-tests))
