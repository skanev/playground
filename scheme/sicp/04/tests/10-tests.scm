(require rackunit rackunit/text-ui)
(load "../10.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.10-tests
  (test-suite
    "Tests for SICP exercise 4.10"

    (check-true (ends-with? 'foo: ':))
    (check-true (ends-with? 'foo:= ':=))
    (check-true (ends-with? 'foo= '=))
    (check-false (ends-with? 'foo ':))
    (check-false (ends-with? 'a ':=))

    (check-equal? (strip-suffix 'foo: ':) 'foo)
    (check-equal? (strip-suffix 'foo:= ':=) 'foo)

    (test-suite "Self-evaluating expressions"
      (check-equal? (run '1) 1)
      (check-equal? (run '"something") "something"))

    (test-suite "Variables"
      (check-equal? (evaluate 'x (extend-environment '(x) '(1) the-empty-environment))
                    1)
      (check-exn exn? (lambda () (evaluate 'x the-empty-environment))))

    (test-suite "Quotation"
      (check-equal? (run '(quote foo)) 'foo))

    (test-suite "Begin"
      (check-equal? (run '(do 1 2)) 2))

    (test-suite "Define"
      (check-equal? (run '(x:= 1)) 'ok)
      (check-equal? (run '(do (x:= 1)
                              x))
                    1)
      (check-equal? (run '(x: () 1)) 'ok)
      (check-equal? (run '(do (x: () 1)
                              (x)))
                    1))

    (test-suite "Set!"
      (check-equal? (run '(do (x:= 1)
                              (x= 2)))
                    'ok)
      (check-equal? (run '(do (x:= 1)
                              (x= 2)
                              x))
                    2))

    (test-suite "If"
      (check-equal? (run '(? true 1 2)) 1)
      (check-equal? (run '(? false 1 2)) 2)
      (check-equal? (run '(? true 1)) 1)
      (check-equal? (run '(? false 1)) false))

    (test-suite "Lambda"
      (check-equal? (run '((() => 1))) 1)
      (check-equal? (run '(((x) => x) 1)) 1)
      (check-equal? (run '(((a b) => (cons a b)) 1 2)) '(1 . 2))
      (check-equal? (run '(do (a:= 1)
                              (b:= 2)
                              (((a) => (cons a b)) 3)))
                    '(3 . 2)))

    (test-suite "Cond"
      (check-equal? (run '(switch (true 1))) 1)
      (check-equal? (run '(switch (false 1) (true 2))) 2)
      (check-equal? (run '(switch (false 1) (else 2))) 2)
      (check-exn exn? (lambda () (run '(switch (else 1) (true 2))))))

    (test-suite "Procedure application"
      (check-equal? (run '(do (a: () 1)
                              (a)))
                    1)
      (check-equal? (run '(do (pair: (a b) (cons a b))
                              (pair 1 2)))
                    '(1 . 2))
      (check-equal? (run '(do (a:= 1)
                              (pair: (b) (cons a b))
                              (pair 2)))
                    '(1 . 2)))

    (test-suite "Defining append"
      (check-equal? (run '(do (append: (x y)
                              (? (null? x)
                                 y
                                 (cons (car x)
                                       (append (cdr x) y))))
                              (append '(a b c) '(d e f))))
                    '(a b c d e f)))

    (test-suite "Factorial"
      (check-equal? (run '(do (factorial: (n)
                                (? (= n 1)
                                   1
                                   (* n (factorial (- n 1)))))
                              (factorial 5)))
                    120)
      (check-equal? (run '(do (factorial: (n)
                                (iter: (n result)
                                  (? (= n 0)
                                     result
                                     (iter (- n 1) (* result n))))
                                (iter n 1))
                              (factorial 5)))
                    120))
))

(run-tests sicp-4.10-tests)
