(require rackunit rackunit/text-ui)
(load "../55.scm")

(load "helpers/query.scm")

(define sicp-4.55-tests
  (test-suite
    "Tests for SICP exercise 4.55"

    (check-equal? (matches-of query-a)
                  '((supervisor (Hacker Alyssa P) (Bitdiddle Ben))
                    (supervisor (Fect Cy D) (Bitdiddle Ben))
                    (supervisor (Tweakit Lem E) (Bitdiddle Ben))))

    (check-equal? (matches-of query-b)
                  '((job (Scrooge Eben) (accounting chief accountant))
                    (job (Cratchet Robert) (accounting scrivener))))

    (check-equal? (matches-of query-c)
                  '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
                    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
                    (address (Aull DeWitt) (Slumerville (Onion Square) 5))))
))

(run-tests sicp-4.55-tests)
