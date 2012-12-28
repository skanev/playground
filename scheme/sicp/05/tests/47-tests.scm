(require rackunit rackunit/text-ui)
(load "helpers/compiler.scm")
(load "../47.scm")

(define (run machine exp)
  (eval-in-machine machine exp)
  (get-register-contents machine 'val))

(define sicp-5.47-tests
  (test-suite
    "Tests for SICP exercise 5.47"

    (test-case "target = val, linkage != return"
      (define machine (make-explicit+compile-machine))
      (compile-in-machine machine '(define (f n) (+ 1 (g n))))
      (eval-in-machine machine '(define (g n) (* n n)))
      (check-equal? (run machine '(f 3)) 10))

    (test-case "target != val, linkage != return"
      (define machine (make-explicit+compile-machine))
      (compile-in-machine machine '(define (twice n) (+ n n)))
      (eval-in-machine machine '(define (get-twice) twice))
      (compile-in-machine machine '(define (ten) ((get-twice) 5)))
      (check-equal? (run machine '(ten)) 10))

    (test-case "target = val, linkage = return"
      (define machine (make-explicit+compile-machine))
      (compile-in-machine machine '(define (twice-1+ n) (twice (+ n 1))))
      (eval-in-machine machine '(define (twice n) (+ n n)))
      (check-equal? (run machine '(twice-1+ 4)) 10))

))

(run-tests sicp-5.47-tests)
