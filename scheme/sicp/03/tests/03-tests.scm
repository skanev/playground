(require rackunit rackunit/text-ui)
(load "../03.scm")

(define sicp-3.03-tests
  (test-suite
    "Tests for SICP exercise 3.03"

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
))

(run-tests sicp-3.03-tests)
