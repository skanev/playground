; EOPL exercise 1.29
;
; (sort loi) returns a list of the elements of loi in ascending order.
;
; > (sort '(8 2 5 2 3))
; (2 2 3 5 8)

(define (merge loi1 loi2)
  (cond ((null? loi1) loi2)
        ((null? loi2) loi1)
        ((< (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2)))
        (#t (cons (car loi2) (merge loi1 (cdr loi2))))))

(define (split loi)
  (define (iter first second n)
    (if (zero? n)
      (list first second)
      (iter (cdr first) (cons (car first) second) (- n 1))))
  (iter loi '() (quotient (length loi) 2)))

(define (merge-sort loi)
  (if (<= (length loi) 1)
    loi
    (let* ((lists (split loi))
           (first (car lists))
           (second (cadr lists)))
      (merge (merge-sort first)
             (merge-sort second)))))

(define (sort loi)
  (merge-sort loi))
