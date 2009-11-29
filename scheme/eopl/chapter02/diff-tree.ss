(module diff-tree eopl
  (provide one diff diff->integer zero is-zero? successor predecessor diff-plus)
  (define one (lambda () '(one)))
  (define diff (lambda (left right) (list 'diff left right)))

  (define diff->integer
    (lambda (diff)
      (if (equal? diff (one))
          1
          (- (diff->integer (cadr diff))
             (diff->integer (caddr diff))))))
  (define zero (lambda () (diff (one) (one))))
  (define is-zero? (lambda (n) (zero? (diff->integer n))))
  (define successor (lambda (n) (diff n (diff (zero) (one)))))
  (define predecessor (lambda (n) (diff n (one))))
  (define diff-plus
    (lambda (left right)
      (if (equal? right (one))
          (successor left)
          (diff left (diff (caddr right) (cadr right))))))

)
