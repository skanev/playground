(require rackunit rackunit/text-ui)
(load "../25.scm")

(define sicp-2.25-tests
  (test-suite
    "Tests for SICP exercise 2.25"

    (check-equal? 
      (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
      7)
    (check-equal?
      (car (car '((7))))
      7)
    (check-equal? 
      (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
      7)
))

(run-tests sicp-2.25-tests)
