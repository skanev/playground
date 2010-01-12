(module node-in-sequence-test scheme
  (require (planet schematics/schemeunit:3))
  (require (planet schematics/schemeunit:3/text-ui))
  (require "node-in-sequence.ss")

  (define node-in-sequence-tests
    (test-suite
      "NodeInSequence representation for lists"

      (check-equal? (number->sequence 7) '(7 () ()))
      (check-equal? (current-element '(6 (5 4 3 2 1) (7 8 9))) 6)
      (check-equal? (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9)))
      (check-equal? (move-to-right '(6 (5 4 3 2 1) (7 8 9))) '(7 (6 5 4 3 2 1) (8 9)))
      (check-equal? (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (13 5 4 3 2 1) (7 8 9)))
      (check-equal? (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (5 4 3 2 1) (13 7 8 9)))
      (check-pred at-left-end? '(7 () (1)))
      (check-false (at-left-end? '(7 (2) (1))))
      (check-pred at-right-end? '(7 (1) ()))
      (check-false (at-right-end? '(7 (2) (1))))
  ))

  (run-tests node-in-sequence-tests)
)
