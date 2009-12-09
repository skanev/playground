; Exercise 2.12
(module stack eopl
  (provide empty-stack push top pop empty-stack?)
   
  (define empty-stack (lambda () (lambda () '())))
  (define push
    (lambda (value stack)
      (lambda () (list value stack))))
  (define pop (lambda (stack) (stack)))
  (define top (lambda (stack) (car (pop stack))))
  (define empty-stack? (lambda (stack) (null? (pop stack))))
)
