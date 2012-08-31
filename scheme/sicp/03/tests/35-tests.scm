(require rackunit rackunit/text-ui)
(load "../35.scm")

(define sicp-3.35-tests
  (test-suite
    "Tests for SICP exercise 3.35"

    (test-case "squarer"
      (define a (make-connector))
      (define b (make-connector))

      (squarer a b)

      (set-value! a 2 'user)
      (check-equal? (get-value b) 4)

      (forget-value! a 'user)
      (set-value! b 4 'user)
      (check-equal? (get-value a) 2))
))

(run-tests sicp-3.35-tests)
