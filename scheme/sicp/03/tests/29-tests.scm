(require rackunit rackunit/text-ui)
(load "../29.scm")

(define sicp-3.29-tests
  (test-suite
    "Tests for SICP exercise 3.29"

    (test-case "or-gate"
      (define input-1 (make-wire))
      (define input-2 (make-wire))
      (define output (make-wire))

      (define (check-or-gate? a b o)
        (set-signal! input-1 a)
        (set-signal! input-2 b)
        (propagate)

        (check-equal? (get-signal output) o))

      (or-gate input-1 input-2 output)

      (check-or-gate? 0 0 0)
      (check-or-gate? 0 1 1)
      (check-or-gate? 1 0 1)
      (check-or-gate? 1 1 1))

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

(run-tests sicp-3.29-tests)
