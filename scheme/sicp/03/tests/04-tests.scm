(require rackunit rackunit/text-ui)
(load "../04.scm")

(define cops-called #f)

(define (call-the-cops)
  (set! cops-called #t))

(define sicp-3.04-tests
  (test-suite
    "Tests for SICP exercise 3.04"

    (before
      (set! cops-called #f)

      (test-begin
        (let ((account (make-account 100 'secret)))
          (check-equal? ((account 'deposit 'secret) 0) 100)
          (check-equal? ((account 'deposit 'secret) 50) 150)
          (check-equal? ((account 'withdraw 'secret) 100) 50)
          (check-equal? ((account 'withdraw 'secret) 100) "Insufficient funds")))

      (test-begin
        (let ((account (make-account 100 'secret)))
          (check-equal? ((account 'deposit 'wrong) 0) "Incorrect password")
          (check-equal? ((account 'deposit 'secret) 0) 100)))

      (test-begin
        (let ((account (make-account 100 'secret)))
          ((account 'withdraw 'wrong) 10)
          ((account 'withdraw 'wrong) 10)
          ((account 'withdraw 'wrong) 10)

          ((account 'withdraw 'secret) 10)

          ((account 'withdraw 'wrong) 10)
          ((account 'withdraw 'wrong) 10)
          ((account 'withdraw 'wrong) 10)
          ((account 'withdraw 'wrong) 10)

          (check-equal? cops-called #f)))

      (test-begin
        (let ((account (make-account 100 'secret)))
          ((account 'withdraw 'wrong) 10)
          ((account 'withdraw 'wrong) 10)
          ((account 'withdraw 'wrong) 10)
          ((account 'withdraw 'wrong) 10)
          ((account 'withdraw 'wrong) 10)
          ((account 'withdraw 'wrong) 10)
          ((account 'withdraw 'wrong) 10)

          (check-equal? cops-called #t))))
))

(run-tests sicp-3.04-tests)
