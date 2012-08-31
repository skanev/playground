(require rackunit rackunit/text-ui)
(load "../37.scm")

(define sicp-3.37-tests
  (test-suite
    "Tests for SICP exercise 3.37"

    (test-case "celsius-fahrenheit-converter"
      (define C (make-connector))
      (define F (celsius-fahrenheit-converter C))

      (set-value! C 25 'user)
      (check-equal? (get-value F) 77)

      (check-exn exn? (lambda () (set-value! F 212 'user)))

      (forget-value! C 'user)
      (set-value! F 212 'user)
      (check-equal? (get-value C) 100))
))

(run-tests sicp-3.37-tests)
