(module focused-bintree-test scheme
  (require (planet schematics/schemeunit:3))
  (require (planet schematics/schemeunit:3/text-ui))
  (require "focused-bintree.ss")

  (define t1 (insert-to-right 14 (insert-to-left 12 (number->bintree 13))))
  (define focused-bintree-tests
    (test-suite
      "Focused binary trees"

      ; Representation
      (check-equal? (number->bintree 42) '(42 () () ()))
      (check-equal? (insert-to-right 14 (number->bintree 12)) '(12 () (14 () ()) ()))
      (check-equal? (insert-to-right 16 (insert-to-right 14 (number->bintree 12))) '(12 () (16 (14 () ()) ()) ()))
      (check-equal? (insert-to-left 14 (number->bintree 12)) '(12 (14 () ()) () ()))
      (check-equal? (insert-to-left 14 (insert-to-left 16 (number->bintree 12))) '(12 (14 (16 () ()) ()) () ()))
      (check-equal? (move-to-left '(1 (2 () ()) (3 () ()) ())) '(2 () () (1 left (3 () ()) ())))
      (check-equal? (move-to-right '(1 (2 () ()) (3 () ()) (4 left () ()))) '(3 () () (1 right (2 () ()) (4 left () ()))))
      (check-equal? (move-up '(1 () () (2 left (3 () ()) (5 right () ())))) '(2 (1 () ()) (3 () ()) (5 right () ())))
      
      ; Interface
      (check-equal? (current-element (number->bintree 42)) 42)
      (check-equal? (current-element t1) 13)
      (check-equal? (current-element (move-to-left t1)) 12)
      (check-equal? (current-element (move-to-right t1)) 14)
      (check-equal? (current-element (move-up (move-to-left t1))) 13)
      (check-pred at-root? t1)
      (check-false (at-root? (move-to-left t1)))
  ))
  (run-tests focused-bintree-tests)
)
