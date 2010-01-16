; Exercise 2.22
(module stack-with-datatype eopl
  (provide empty-stack push
           top pop
           empty-stack?)

  (define-datatype stack stack?
    (empty-stack)
    (push
      (value integer?)
      (nested-stack stack?)))

  (define empty-stack?
    (lambda (s)
      (cases stack s
        (empty-stack () #t)
        (else #f))))

  (define top
    (lambda (s)
      (cases stack s
        (empty-stack () (eopl:error "Stack is empty"))
        (push (value nested) value))))

  (define pop
    (lambda (s)
      (cases stack s
        (empty-stack () (eopl:error "Stack is empty"))
        (push (value nested) nested))))
)
