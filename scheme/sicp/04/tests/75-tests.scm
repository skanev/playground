(require rackunit rackunit/text-ui)
(load "helpers/query.scm")
(load "../75.scm")

(define sicp-4.75-tests
  (test-suite
    "Tests for SICP exercise 4.75"

    (check-equal? (matches-of '(unique (job ?x (computer wizard))))
                  '((unique (job (Bitdiddle Ben) (computer wizard)))))

    (check-equal? (matches-of '(and (job ?x ?j) (unique (job ?anyone ?j))))
                  '((and (job (Warbucks Oliver) (administration big wheel))
                         (unique (job (Warbucks Oliver) (administration big wheel))))
                    (and (job (Bitdiddle Ben) (computer wizard))
                         (unique (job (Bitdiddle Ben) (computer wizard))))
                    (and (job (Tweakit Lem E) (computer technician))
                         (unique (job (Tweakit Lem E) (computer technician))))
                    (and (job (Reasoner Louis) (computer programmer trainee))
                         (unique (job (Reasoner Louis) (computer programmer trainee))))
                    (and (job (Scrooge Eben) (accounting chief accountant))
                         (unique (job (Scrooge Eben) (accounting chief accountant))))
                    (and (job (Cratchet Robert) (accounting scrivener))
                         (unique (job (Cratchet Robert) (accounting scrivener))))
                    (and (job (Aull DeWitt) (administration secretary))
                         (unique (job (Aull DeWitt) (administration secretary))))))

    (check-equal? (matches-of supervises-one-person)
                  '((and (supervisor (Reasoner Louis) (Hacker Alyssa P))
                         (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))
                    (and (supervisor (Cratchet Robert) (Scrooge Eben))
                         (unique (supervisor (Cratchet Robert) (Scrooge Eben))))))
))

(run-tests sicp-4.75-tests)
