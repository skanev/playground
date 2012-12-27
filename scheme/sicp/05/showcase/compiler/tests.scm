(require rackunit rackunit/text-ui)

(load "helpers.scm")

(define (run exp)
  (let ((machine (make-explicit+compile-machine)))
    (compile-in-machine machine exp)
    (get-register-contents machine 'val)))

(define evaluator-tests
  (test-suite
    "Tests for the metacircular evaluator"

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
))

(run-tests evaluator-tests)
