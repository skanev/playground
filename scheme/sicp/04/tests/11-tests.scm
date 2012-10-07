(require rackunit rackunit/text-ui)
(load "../11.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.11-tests
  (test-suite
    "Tests for SICP exercise 4.11"

    (check-equal? (make-frame '(a b c) '(1 2 3))
                  '(frame (a . 1) (b . 2) (c . 3)))
    (check-equal? (frame-variables (make-frame '(a b c) '(1 2 3)))
                  '(a b c))
    (check-equal? (frame-values (make-frame '(a b c) '(1 2 3)))
                  '(1 2 3))

    (check-equal? (extend-environment '(a b) '(1 2)
                                      (extend-environment '(a c) '(3 4)
                                                          the-empty-environment))
                  '((frame (a . 1) (b . 2))
                    (frame (a . 3) (c . 4))))

    (check-equal? (lookup-variable-value 'a
                    (extend-environment '(a) '(1) the-empty-environment))
                  1)
    (check-equal? (lookup-variable-value 'a
                    (extend-environment '(b) '(1)
                      (extend-environment '(a) '(2) the-empty-environment)))
                  2)
    (check-equal? (lookup-variable-value 'b
                    (extend-environment '(a b) '(1 2) the-empty-environment))
                  2)
    (check-equal? (lookup-variable-value 'c
                    (extend-environment '(a b) '(1 2)
                      (extend-environment '(a c) '(3 4) the-empty-environment)))
                  4)
    (check-equal? (lookup-variable-value 'a
                    (extend-environment '(a b) '(1 2)
                      (extend-environment '(a c) '(3 4) the-empty-environment)))
                  1)

    (test-begin
      (define frame (make-frame '(a b) '(1 2)))
      (add-binding-to-frame! 'c 3 frame)
      (check-equal? frame '(frame (c . 3) (a . 1) (b . 2))))

    (test-begin
      (define env (extend-environment '(a b) '(1 2) the-empty-environment))
      (set-variable-value! 'b 3 env)
      (check-equal? (lookup-variable-value 'b env) 3))

    (test-begin
      (define env (extend-environment '(a b) '(1 2)
                    (extend-environment '(a c) '(3 4) the-empty-environment)))
      (set-variable-value! 'c 5 env)
      (check-equal? env
                    '((frame (a . 1) (b . 2))
                      (frame (a . 3) (c . 5)))))

    (test-begin
      (define env (extend-environment '(a b) '(1 2) the-empty-environment))
      (define-variable! 'c 3 env)
      (check-equal? env '((frame (c . 3) (a . 1) (b . 2)))))

    (test-begin
      (define env (extend-environment '(a b) '(1 2) the-empty-environment))
      (define-variable! 'b 3 env)
      (check-equal? env '((frame (a . 1) (b . 3)))))
))

(run-tests sicp-4.11-tests)
