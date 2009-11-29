(module bigint-test scheme
  (require (planet schematics/schemeunit:3))
  (require (planet schematics/schemeunit:3/text-ui))
  (require "bigint.ss")

  (define (make-bigint n)
    (let mk-bigint ([a n] [b (zero)])
      (if (zero? a) b (mk-bigint (sub1 a) (successor b)))))

  (define bigint-tests
    (test-suite
      "Bigints"

      (check-equal? (zero) '() "Zero is '()")
      (check-pred is-zero? (zero) "(zero) is-zero?")

      (check-false (is-zero? (successor (zero))))
      (check-equal? (successor (zero)) '(1))
      (check-equal? (make-bigint N) '(0 1))
      (check-equal? (make-bigint (expt N 3)) '(0 0 0 1))

      (check-pred is-zero? (predecessor (successor (zero))))
      (check-exn exn? (lambda () (predecessor (zero))))
      (check-equal? (predecessor '(1)) '())
      (check-equal? (predecessor '(2)) '(1))
      (check-equal? (predecessor '(0 1)) (list (sub1 N)))
      (check-equal? (predecessor '(0 0 1)) (list (sub1 N) (sub1 N)))
  ))

  (run-tests bigint-tests)
)
