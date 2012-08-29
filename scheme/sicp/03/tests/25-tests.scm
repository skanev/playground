(require rackunit rackunit/text-ui)
(load "../25.scm")

(define sicp-3.25-tests
  (test-suite
    "Tests for SICP exercise 3.25"

    (test-begin "lookup"
      (check-equal? (lookup '(inexistant) (make-table)) #f))

    (test-begin "insert! (simple)"
      (define table (make-table))

      (insert! '(1) 'one table)
      (insert! '(2) 'two table)

      (check-equal? (lookup '(1) table) 'one)
      (check-equal? (lookup '(2) table) 'two))

    (test-begin "insert! (two levels)"
      (define table (make-table))

      (insert! '(1 1) 'eleven table)
      (insert! '(1 2) 'twelve table)
      (insert! '(2 1) 'twenty-one table)
      (insert! '(2 2) 'twenty-two table)

      (check-equal? (lookup '(1 1) table) 'eleven)
      (check-equal? (lookup '(1 2) table) 'twelve)
      (check-equal? (lookup '(2 1) table) 'twenty-one)
      (check-equal? (lookup '(2 2) table) 'twenty-two))

    (test-begin "insert! (mixed levels)"
      (define table (make-table))

      (insert! '(1) 'one table)
      (insert! '(2 1) 'two-one table)
      (insert! '(2 2) 'two-two table)
      (insert! '(3 1 1) 'three-one-one table)
      (insert! '(3 1 2) 'three-one-two table)
      (insert! '(3 2 1) 'three-two-one table)
      (insert! '(3 2 2) 'three-two-two table)

      (check-equal? (lookup '(1) table) 'one)
      (check-equal? (lookup '(2 1) table) 'two-one)
      (check-equal? (lookup '(2 2) table) 'two-two)
      (check-equal? (lookup '(3 1 1) table) 'three-one-one)
      (check-equal? (lookup '(3 1 2) table) 'three-one-two)
      (check-equal? (lookup '(3 2 1) table) 'three-two-one)
      (check-equal? (lookup '(3 2 2) table) 'three-two-two))
))

(run-tests sicp-3.25-tests)
