; SICP exercise 2.39
;
; Complete the following definitions of reverse (exercise 2.18) in terms of
; fold-right and fold-left from exercise 2.38:
;
; (define (reverse sequence)
;   (fold-right (lambda (x y) <??>) nil sequence))
;
; (define (reverse sequence)
;   (fold-left (lambda (x y) <??>) nil sequence))

(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x)))
              nil
              sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x))
             nil
             sequence))



(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define nil '())
