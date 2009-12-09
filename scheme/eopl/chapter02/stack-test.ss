(module stack-test scheme
  (require (planet schematics/schemeunit:3))
  (require (planet schematics/schemeunit:3/text-ui))
  (require "stack.ss")


  (define stack-tests
    (test-suite
      "Procedural stack representation"
      
      (check-equal? (car (pop (push 'a (empty-stack)))) 'a)
      (check-equal? (car (pop (push 'b (push 'a (empty-stack))))) 'b)
      (check-equal? (car (pop (cadr (pop (push 'b (push 'a (empty-stack))))))) 'a)
      (check-equal? (top (push 'a (empty-stack))) 'a)
      (check-pred empty-stack? (empty-stack))
      (check-false (empty-stack? (push 'a (empty-stack))))
  ))

  (run-tests stack-tests)
)
