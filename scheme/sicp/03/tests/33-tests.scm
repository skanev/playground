(require rackunit rackunit/text-ui)
(load "../33.scm")

(define sicp-3.33-tests
  (test-suite
    "Tests for SICP exercise 3.33"

    (test-case "averager"
      (define a (make-connector))
      (define b (make-connector))
      (define c (make-connector))

      (averager a b c)

      (set-value! a 4 'user)
      (set-value! b 6 'user)
      (check-equal? (get-value c) 5)

      (forget-value! b 'user)
      (set-value! c 5 'user)
      (check-equal? (get-value b) 6)

      (forget-value! a 'user)
      (set-value! b 6 'user)
      (check-equal? (get-value a) 4))

    (test-case "celsius-fahrenheit-converter"
      (define C (make-connector))
      (define F (make-connector))
      (celsius-fahrenheit-converter C F)

      (set-value! C 25 'user)
      (check-equal? (get-value F) 77)

      (check-exn exn? (lambda () (set-value! F 212 'user)))

      (forget-value! C 'user)
      (set-value! F 212 'user)
      (check-equal? (get-value C) 100))
))

(run-tests sicp-3.33-tests)
