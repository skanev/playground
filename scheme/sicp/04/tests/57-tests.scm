(require rackunit rackunit/text-ui)
(load "helpers/query.scm")
(load "../57.scm")

(define sicp-4.57-tests
  (test-suite
    "Tests for SICP exercise 4.57"

    (check-equal? (matches-of query-a)
                  '((can-replace (Bitdiddle Ben) (Fect Cy D))))

    (check-equal? (matches-of query-b)
                  '((and (can-replace (Aull DeWitt) (Warbucks Oliver))
                         (salary (Aull DeWitt) 25000)
                         (salary (Warbucks Oliver) 150000)
                         (lisp-value < 25000 150000))))

))

(run-tests sicp-4.57-tests)
