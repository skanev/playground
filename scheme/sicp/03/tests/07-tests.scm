(require rackunit rackunit/text-ui)
(load "../07.scm")

(define sicp-3.07-tests
  (test-suite
    "Tests for SICP exercise 3.07"

    (test-begin
      (let* ((peter-acc (make-account 100 'open-sesame))
             (paul-acc (make-joint peter-acc 'open-sesame 'rosebud)))
        ((paul-acc 'deposit 'rosebud) 10)
        ((paul-acc 'withdraw 'rosebud) 50)
        (check-equal? ((peter-acc 'deposit 'open-sesame) 0) 60)))

    (test-begin
      (let* ((peter-acc (make-account 100 'open-sesame))
             (paul-acc (make-joint peter-acc 'open-sesame 'rosebud)))
        (check-equal? ((paul-acc 'deposit 'wrong) 0) "Incorrect password")))
))

(run-tests sicp-3.07-tests)
