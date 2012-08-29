(require rackunit rackunit/text-ui)
(load "../24.scm")

(define sicp-3.24-tests
  (test-suite
    "Tests for SICP exercise 3.24"

    (test-begin "lookup"
      (check-equal? (lookup 'inexistant (make-table equal?)) #f))

    (test-begin "insert!"
      (define table (make-table equal?))

      (insert! 1 'one table)
      (insert! 2 'two table)

      (check-equal? (lookup 1 table) 'one)
      (check-equal? (lookup 2 table) 'two))

    (test-begin "make-table with a lambda"
      (define table (make-table (lambda (a b) (= (remainder a 10) (remainder b 10)))))

      (insert! 11 'one table)

      (check-equal? (lookup 11 table) 'one)
      (check-equal? (lookup 21 table) 'one)
      (check-equal? (lookup 1 table) 'one))
))

(run-tests sicp-3.24-tests)
