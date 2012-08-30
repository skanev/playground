(require rackunit rackunit/text-ui)
(load "../30.scm")

(define sicp-3.30-tests
  (test-suite
    "Tests for SICP exercise 3.30"

    (test-case "ripple-carry adder"
      (define addend1 (list (make-wire) (make-wire) (make-wire)))
      (define addend2 (list (make-wire) (make-wire) (make-wire)))
      (define sum (list (make-wire) (make-wire) (make-wire)))
      (define carry-in (make-wire))
      (define carry-out (make-wire))

      (define (digits n)
        (define (bits n r)
          (if (= r 0)
              '()
              (cons (remainder n 2)
                    (bits (quotient n 2) (- r 1)))))
        (bits n 3))

      (define (set-signals! wires signals)
        (if (null? signals)
            'done
            (begin (set-signal! (car wires) (car signals))
                   (set-signals! (cdr wires) (cdr signals)))))

      (define (check-adder a b s c-in c-out)
        (set-signals! addend1 (digits a))
        (set-signals! addend2 (digits b))
        (set-signal! carry-in c-in)

        (propagate)

        (check-equal? (map get-signal sum) (digits s))
        (check-equal? (get-signal carry-out) c-out))

      (ripple-carry-adder addend1 addend2 carry-in sum carry-out)

      (check-adder 0 0 0 0 0)
      (check-adder 1 2 3 0 0)
      (check-adder 2 4 6 0 0)
      (check-adder 0 0 1 1 0)
      (check-adder 3 3 7 1 0)
      (check-adder 3 4 0 1 1))


))

(run-tests sicp-3.30-tests)
