(require rackunit rackunit/text-ui)
(load "helpers/compiler.scm")
(load "../44.scm")

(define (run exp)
  (let ((machine (make-explicit+compile-machine)))
    (compile-in-machine machine exp)
    (get-register-contents machine 'val)))

(define sicp-5.44-tests
  (test-suite
    "Tests for SICP exercise 5.44"

    (check-equal? (run '(begin (define (vect+ v1 v2)
                                 (cons (+ (car v1) (car v2))
                                       (+ (cdr v1) (cdr v2))))
                               (define (vect* n v)
                                 (cons (* n (car v))
                                       (* n (cdr v))))
                               (define (linear + * a b x y)
                                 (+ (* a x) (* b y)))
                               (linear vect+ vect*
                                       2 3
                                       '(5 . 7) '(11 . 13))))
                  '(43 . 53))
))

(run-tests sicp-5.44-tests)
