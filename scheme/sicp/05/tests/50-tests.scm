(require rackunit rackunit/text-ui)
(require r5rs/init)
(load "helpers/compiler.scm")
(load "../50.scm")

(define (run exp)
  (compile-in-machine machine `(evaluate ',exp the-global-environment))
  (get-register-contents machine 'val))

(define sicp-5.50-tests
  (test-suite
    "Tests for SICP exercise 5.50"

    (test-suite "Self-evaluating expressions"
      (check-equal? (run '1) 1)
      (check-equal? (run '"something") "something"))

    (test-suite "Quotation"
      (check-equal? (run '(quote foo)) 'foo))

    (test-suite "Begin"
      (check-equal? (run '(begin 1 2)) 2))

    (test-suite "Define"
      (check-equal? (run '(define x 1)) 'ok)
      (check-equal? (run '(begin (define x 1)
                                 x))
                    1)
      (check-equal? (run '(define (x) 1)) 'ok)
      (check-equal? (run '(begin (define (x) 1)
                                 (x)))
                    1))

    (test-suite "Set!"
      (check-equal? (run '(begin (define x 1)
                                 (set! x 2)))
                    'ok)
      (check-equal? (run '(begin (define x 1)
                                 (set! x 2)
                                 x))
                    2))

    (test-suite "If"
      (check-equal? (run '(if true 1 2)) 1)
      (check-equal? (run '(if false 1 2)) 2)
      (check-equal? (run '(if true 1)) 1)
      (check-equal? (run '(if false 1)) false))

    (test-suite "Lambda"
      (check-equal? (run '((lambda () 1))) 1)
      (check-equal? (run '((lambda (x) x) 1)) 1)
      (check-equal? (run '((lambda (a b) (cons a b)) 1 2)) '(1 . 2))
      (check-equal? (run '(begin (define a 1)
                                 (define b 2)
                                 ((lambda (a) (cons a b)) 3)))
                    '(3 . 2)))

    (test-suite "Cond"
      (check-equal? (run '(cond (true 1))) 1)
      (check-equal? (run '(cond (false 1) (true 2))) 2)
      (check-equal? (run '(cond (false 1) (else 2))) 2)
      (check-exn exn? (lambda () (run '(cond (else 1) (true 2))))))

    (test-suite "Procedure application"
      (check-equal? (run '(begin (define (a) 1)
                                 (a)))
                    1)
      (check-equal? (run '(begin (define (pair a b) (cons a b))
                                 (pair 1 2)))
                    '(1 . 2))
      (check-equal? (run '(begin (define a 1)
                                 (define (pair b) (cons a b))
                                 (pair 2)))
                    '(1 . 2)))

    (test-suite "Defining append"
      (check-equal? (run '(begin (define (append x y)
                                   (if (null? x)
                                       y
                                       (cons (car x)
                                             (append (cdr x) y))))
                                 (append '(a b c) '(d e f))))
                    '(a b c d e f)))

    (test-suite "Factorial"
      (check-equal? (run '(begin (define (factorial n)
                                   (if (= n 1)
                                       1
                                       (* n (factorial (- n 1)))))
                                 (factorial 5)))
                    120)
      (check-equal? (run '(begin (define (factorial n)
                                   (define (iter n result)
                                     (if (= n 0)
                                         result
                                         (iter (- n 1) (* result n))))
                                   (iter n 1))
                                 (factorial 5)))
                    120))

    ; Those are just some sanity checks. It is not necessary that it takes
    ; that many instructions and stack operations to run the tests, but makes
    ; sure that if I did not short-circuit something when this number changes.

    (check-equal? total-instructions 215524)
    (check-equal? ((machine 'stack) 'statistics)
                  '(total-pushes = 8036 maximum-depth = 22))

    ; The numbers get way cooler when we do a lame recursive version of
    ; Fibonacci 10.

    (test-case "The tenth Fibonacci number"
      (set! total-instructions 0)
      ((machine 'stack) 'initialize)

      (check-equal? (run '(begin (define (fib n)
                                   (if (< n 2)
                                       1
                                       (+ (fib (- n 1)) (fib (- n 2)))))
                                 (fib 10)))
                    89)

      (check-equal? total-instructions 1561470)
      (check-equal? ((machine 'stack) 'statistics)
                    '(total-pushes = 190611 maximum-depth = 76)))
))

(run-tests sicp-5.50-tests)
