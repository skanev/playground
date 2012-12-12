(require rackunit rackunit/text-ui)
(load "helpers/query.scm")
(load "../63.scm")

(define sicp-4.63-tests
  (test-suite
    "Tests for SICP exercise 4.63"

    (check-equal? (matches-of grandson-of-cain)
                  '((grandson Cain Irad)))

    (check-equal? (matches-of sons-of-lamech)
                  '((son Lamech Jabal)
                    (son Lamech Jubal)))

    (check-equal? (matches-of grandsons-of-methushael)
                  '((grandson Methushael Jabal)
                    (grandson Methushael Jubal)))

))

(run-tests sicp-4.63-tests)
