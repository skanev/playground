; Exercise 2.18
(module node-in-sequence eopl
  (provide number->sequence current-element move-to-left move-to-right
           insert-to-left insert-to-right at-left-end? at-right-end?)

  (define number->sequence (lambda (n) (list n '() '())))
  (define current-element (lambda (seq) (car seq)))
  (define move-to-left
    (lambda (seq) 
      (let [(current (car seq))
            (left (cadr seq))
            (right (caddr seq))]
        (list (car left) (cdr left) (cons current right)))))
  (define move-to-right
    (lambda (seq)
      (let [(current (car seq))
            (left (cadr seq))
            (right (caddr seq))]
        (list (car right) (cons current left) (cdr right)))))
  (define insert-to-left
    (lambda (num seq)
      (let [(current (car seq))
            (left (cadr seq))
            (right (caddr seq))]
        (list current (cons num left) right))))
  (define insert-to-right
    (lambda (num seq)
      (let [(current (car seq))
            (left (cadr seq))
            (right (caddr seq))]
        (list current left (cons num right)))))
  (define at-left-end? (lambda (seq) (null? (cadr seq))))
  (define at-right-end? (lambda (seq) (null? (caddr seq))))
)
