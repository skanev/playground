(module bintree-test scheme
  (require (planet schematics/schemeunit:3))
  (require (planet schematics/schemeunit:3/text-ui))
  (require "bintree.ss")

  (define bintree-tests
    (test-suite
      "Binary trees"
      (check-equal? (number->bintree 42) '(42 () ()))
      (check-equal? (current-element '(42 () ())) 42)
      (check-pred at-leaf? '())
      (check-false (at-leaf? '(12 () ())))
      (check-equal? (move-to-left-son '(13 (12 () ()) (14 () ()))) '(12 () ()))
      (check-equal? (move-to-right-son '(13 (12 () ()) (14 () ()))) '(14 () ()))
      (check-equal?
        (insert-to-right 14 (insert-to-left 12 (number->bintree 13)))
        '(13 (12 () ()) (14 () ())))
      (check-equal?
        (insert-to-left 15 '(13 (12 () ()) (14 () ())))
        '(13 (15 (12 () ()) ()) (14 () ())))
  ))

  (run-tests bintree-tests)
)
