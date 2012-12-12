(require rackunit rackunit/text-ui)
(load "helpers/query.scm")
(load "../60.scm")

(define sicp-4.60-tests
  (test-suite
    "Tests for SICP exercise 4.60"

    (check-equal? (matches-of '(ordered-neighbour-pair ?person-1 ?person-2))
                  '((ordered-neighbour-pair (Bitdiddle Ben) (Reasoner Louis))
                    (ordered-neighbour-pair (Fect Cy D) (Hacker Alyssa P))
                    (ordered-neighbour-pair (Aull DeWitt) (Bitdiddle Ben))
                    (ordered-neighbour-pair (Aull DeWitt) (Reasoner Louis))))
))

(run-tests sicp-4.60-tests)
