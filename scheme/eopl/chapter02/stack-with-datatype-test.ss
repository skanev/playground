(module stack-with-datatype-test scheme
  (require (planet schematics/schemeunit:3)
           (planet schematics/schemeunit:3/text-ui)
           "stack-with-datatype.ss")

  (define stack-with-datatype-tests
    (test-suite
      "Stack implemented with define-datatype"
      
      ; empty-stack push pop top empty-stack?
      (check-pred empty-stack? (empty-stack) "empty-stack? -> true")
      (check-false (empty-stack? (push 10 (empty-stack))) "empty-stack? -> false")
      (check-equal? (top (push 10 (empty-stack))) 10 "top")
      (check-exn exn? (lambda () (top (empty-stack))) "(top (empty-stack)) raises exception")
      (check-equal? (top (pop (push 10 (push 42 (empty-stack))))) 42 "pop")
      (check-exn exn? (lambda () (pop (empty-stack))) "(pop (emtpy-stack)) raises exception")
  ))

  (run-tests stack-with-datatype-tests)
)
