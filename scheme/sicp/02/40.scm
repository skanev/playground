; SICP exercise 2.40
;
; Define a procedure unique-pairs that, given an integer n, generates the
; sequence of pairs (i, j) with 1 ≤ j < i ≤ n. Use unique-pairs to simplify the
; definition of prime-sum-pairs given above.

(define (unique-pairs n)
  (flatmap (lambda (a)
             (map (lambda (b) (list a b))
                  (enumerate-interval (+ a 1) n)))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (filter (lambda (pair)
            (prime? (+ (car pair)
                       (cadr pair))))
          (unique-pairs n)))




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

(define (prime? n)
  (null?
    (filter (lambda (x) (= 0 (remainder n x)))
            (enumerate-interval 2 (- n 1)))))

(define nil '())
