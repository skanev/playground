(require rackunit rackunit/text-ui)
(load "../28.scm")

(define sicp-3.28-tests
  (test-suite
    "Tests for SICP exercise 3.28"

    (test-case "half-adder"
      (define input-1 (make-wire))
      (define input-2 (make-wire))
      (define sum (make-wire))
      (define carry (make-wire))

      (with-output-to-string
        (lambda ()
          (probe 'sum sum)
          (probe 'carry carry)))

      (half-adder input-1 input-2 sum carry)

      (set-signal! input-1 1)
      (check-equal? "sum 8  New-value = 1\n"
                    (with-output-to-string propagate))

      (set-signal! input-2 1)
      (check-equal? (string-append "carry 11  New-value = 1\n"
                                   "sum 16  New-value = 0\n")
                    (with-output-to-string propagate)))

    (test-case "full-adder"
      (define input-1 (make-wire))
      (define input-2 (make-wire))
      (define carry-in (make-wire))
      (define sum (make-wire))
      (define carry-out (make-wire))

      (define (check-adder? a b c-in c-out s)
        (set-signal! input-1 a)
        (set-signal! input-2 b)
        (set-signal! carry-in c-in)
        (propagate)

        (check-equal? (list (get-signal sum) (get-signal carry-out))
                      (list s c-out)))

      (full-adder input-1 input-2 carry-in sum carry-out)

      (check-adder? 0 0 0 0 0)
      (check-adder? 0 1 0 0 1)
      (check-adder? 1 0 0 0 1)
      (check-adder? 1 1 0 1 0)
      (check-adder? 0 0 1 0 1)
      (check-adder? 0 1 1 1 0)
      (check-adder? 1 0 1 1 0)
      (check-adder? 1 1 1 1 1))
))

(run-tests sicp-3.28-tests)
