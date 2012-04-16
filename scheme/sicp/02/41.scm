; SICP exercise 2.41
;
; Write a procedure to find all ordered priples of distinct positive integers
; i, j, and k less than or equal to a given integer n that sums to a given
; integer s.

(define (triples-sum n s)
  (filter (lambda (triple) (= s (sum triple)))
          (enumerate-triples n)))

(define (enumerate-triples n)
  (flatmap (lambda (a)
             (flatmap (lambda (b)
                        (map (lambda (c) (list a b c))
                             (enumerate-interval (+ b 1) n)))
                      (enumerate-interval (+ a 1) n)))
           (enumerate-interval 1 n)))



(define (sum numbers)
  (accumulate + 0 numbers))

(define (enumerate-interval a b)
  (if (> a b)
    (list)
    (cons a
          (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define nil '())
