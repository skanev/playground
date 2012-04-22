; SICP exercise 2.65
;
; Use the results of exercise 2.63 and 2.64 to give Θ(n) implementations of
; union-set and intersection-set for sets implemented as (balanced) binary
; trees.

(define (intersection-set set1 set2)
  (list->tree (intersection-set-list (tree->list set1)
                                     (tree->list set2))))

(define (union-set set1 set2)
  (list->tree (union-set-list (tree->list set1)
                              (tree->list set2))))



(define (make-tree entry left right) (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let* ((left-size (quotient (- n 1) 2))
               (left-result (partial-tree elts left-size))
               (left-tree (car left-result))
               (non-left-elts (cdr left-result))
               (right-size (- n (+ left-size 1)))
               (this-entry (car non-left-elts))
               (right-result (partial-tree (cdr non-left-elts) right-size))
               (right-tree (car right-result))
               (remaining-elts (cdr right-result)))
          (cons (make-tree this-entry left-tree right-tree) remaining-elts))))
  (car (partial-tree elements (length elements))))

(define (union-set-list list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        ((= (car list1) (car list2))
         (cons (car list1) (union-set-list (cdr list1) (cdr list2))))
        ((< (car list1) (car list2))
         (cons (car list1) (union-set-list (cdr list1) list2)))
        ((> (car list1) (car list2))
         (cons (car list2) (union-set-list list1 (cdr list2))))))

(define (intersection-set-list list1 list2)
  (if (or (null? list1) (null? list2))
      '()
      (let ((x1 (car list1))
            (x2 (car list2)))
        (cond ((= x1 x2) (cons x1 (intersection-set-list (cdr list1) (cdr list2))))
              ((< x1 x2) (intersection-set-list (cdr list1) list2))
              ((> x1 x2) (intersection-set-list list1 (cdr list2)))))))
