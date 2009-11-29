; Exercise 2.1
(module bigint eopl
  (provide N zero is-zero? successor predecessor)

  (define N 100)
  (define zero (lambda () '()))
  (define is-zero? (lambda (n) (equal? n '())))
  (define successor
    (lambda (n) 
      (cond [(is-zero? n) '(1)]
            [(equal? (- N 1) (car n)) (cons 0 (successor (cdr n)))]
            [else (cons (+ (car n) 1) (cdr n))])))
  (define predecessor
    (lambda (n)
      (cond [(is-zero? n) (eopl:error "Zero has no predecessors")]
            [(equal? n '(1)) (zero)]
            [(zero? (car n)) (cons (- N 1) (predecessor (cdr n)))]
            [else (cons (- (car n) 1) (cdr n))])))
)   
