(require rackunit rackunit/text-ui)
(load "../56.scm")

(load "helpers/query.scm")

(define sicp-4.56-tests
  (test-suite
    "Tests for SICP exercise 4.56"

    (check-equal? (matches-of query-a)
                  '((and (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
                         (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
                    (and (address (Fect Cy D) (Cambridge (Ames Street) 3))
                         (supervisor (Fect Cy D) (Bitdiddle Ben)))
                    (and (address (Tweakit Lem E) (Boston (Bay State Road) 22))
                         (supervisor (Tweakit Lem E) (Bitdiddle Ben)))))

    (check-equal? (matches-of query-b)
                  '((and (salary (Hacker Alyssa P) 40000)
                         (salary (Bitdiddle Ben) 60000)
                         (lisp-value < 40000 60000))
                    (and (salary (Fect Cy D) 35000)
                         (salary (Bitdiddle Ben) 60000)
                         (lisp-value < 35000 60000))
                    (and (salary (Tweakit Lem E) 25000)
                         (salary (Bitdiddle Ben) 60000)
                         (lisp-value < 25000 60000))
                    (and (salary (Reasoner Louis) 30000)
                         (salary (Bitdiddle Ben) 60000)
                         (lisp-value < 30000 60000))
                    (and (salary (Cratchet Robert) 18000)
                         (salary (Bitdiddle Ben) 60000)
                         (lisp-value < 18000 60000))
                    (and (salary (Aull DeWitt) 25000)
                         (salary (Bitdiddle Ben) 60000)
                         (lisp-value < 25000 60000))))

    (check-equal? (matches-of query-c)
                  '((and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
                         (not (job (Warbucks Oliver) (computer . ?title))))
                    (and (supervisor (Scrooge Eben) (Warbucks Oliver))
                         (not (job (Warbucks Oliver) (computer . ?title))))
                    (and (supervisor (Cratchet Robert) (Scrooge Eben))
                         (not (job (Scrooge Eben) (computer . ?title))))
                    (and (supervisor (Aull DeWitt) (Warbucks Oliver))
                         (not (job (Warbucks Oliver) (computer . ?title))))))
))

(run-tests sicp-4.56-tests)
