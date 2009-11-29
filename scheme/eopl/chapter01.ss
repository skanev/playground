(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (length (cdr lst))))))

(list-length '(1 2 3 4 5 6))




(define nth-element
  (lambda (lst n)
    (if (zero? n)
        (car lst)
        (nth-element (cdr lst) (- n 1)))))

(nth-element '(a b c d e f) 3)




(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? s (car los))
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

(remove-first 'a '(a b c))
(remove-first 'b '(e f g))
(remove-first 'a4 '(c1 a4 c1 a4))
(remove-first 'x '())




(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))
    
(remove 'a '(a b c d a b c d))
(remove 'b '(a c d))
(remove 'a '(a a a))
(remove 'a '(b c d))




(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? 'lambda (car exp))
       (and
         (not (eqv? var (car (cadr exp))))
         (occurs-free? var (caddr exp))))
      (else
       (or
        (occurs-free? var (car exp))
        (occurs-free? var (cadr exp)))))))

(occurs-free? 'x 'x)
(occurs-free? 'x 'y)
(occurs-free? 'x '(lambda (x) (x y)))
(occurs-free? 'x '(lambda (y) (x y)))
(occurs-free? 'x '((lambda (x) x) (x y)))
(occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))))




(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (subst-in-s-exp new old (car slist))
         (subst new old (cdr slist))))))

(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? old sexp) new sexp)
        (subst new old sexp))))

(subst 'a 'b '((b c) (b () d e)))




(define subst-inline
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (cond ((null? (car slist)) '())
               ((symbol? (car slist)) (if (eqv? old (car slist)) new (car slist)))
               (else (subst-inline new old (car slist))))
         (subst-inline new old (cdr slist))))))

(subst-inline 'a 'b '((b c) (b () d)))




(define subst-map
  (lambda (new old slist)
    (map (lambda (sexp) (subst-in-s-exp new old sexp)) slist)))

(subst-map 'a 'b '((b c) (b () d e b g)))



(define number-elements-from
  (lambda (lst n)
    (if (null? lst)
        '()
        (cons
         (list n (car lst))
         (number-elements-from (cdr lst) (+ n 1))))))

(define number-elements
  (lambda (lst) (number-elements-from lst 0)))

(number-elements '(a b c d e))

; Exercises
(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))

(duple 2 3)
(duple 4 '(ha ha))
(duple 0 '(blah))



(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons
         (list (cadar lst) (caar lst))
         (invert (cdr lst))))))

(invert '((a 1) (a 2) (1 b) (2 b)))



(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst)) (down (cdr lst))))))
    
(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))



(define swapper
  (lambda (s1 s2 slist)
    (cond ((null? slist) '())
          ((symbol? slist) (cond ((eqv? slist s1) s2)
                                 ((eqv? slist s2) s1)
                                 (else slist)))
          (else (cons (swapper s1 s2 (car slist)) (swapper s1 s2 (cdr slist)))))))

(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))



(define list-set
  (lambda (lst n x)
    (if (zero? n)
        (cons x (cdr lst))
        (cons (car lst) (list-set (cdr lst) (- n 1) x)))))

(list-set '(a b c d) 2 '(1 2))
(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)



(define count-occurences
  (lambda (s slist)
    (cond ((null? slist) 0)
          ((symbol? slist) (if (eqv? slist s) 1 0))
          (else (+ (count-occurences s (car slist)) (count-occurences s (cdr slist)))))))

(count-occurences 'x '((f x) y (((x z) x))))
(count-occurences 'x '((f x) y (((x z) () x))))
(count-occurences 'w '((f x) y (((x z) x))))



(define product
  (lambda (sos1 sos2)
    (cond ((null? sos1) '())
          ((null? sos2) '())
          ((null? (cdr sos1))
             (cons (list (car sos1) (car sos2)) (product sos1 (cdr sos2))))
          (else (append (product (list (car sos1)) sos2) (product (cdr sos1) sos2)))))) 
           
(product '(a b c) '(x y))
(product '(a b c) '(x y z))



(define filter-in
  (lambda (pred lst)
    (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter-in pred (cdr lst))))
        (else (filter-in pred (cdr lst))))))

(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))



(define list-index
  (lambda (pred lst)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) 0)
      (else 
       (let ((res (list-index pred (cdr lst))))
         (if (eq? #f res) #f (+ 1 res)))))))

(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))



(define every?
  (lambda (pred lst)
    (cond 
      ((null? lst) #t)
      ((not (pred (car lst))) #f)
      (else (every? pred (cdr lst))))))

(every? number? '(a b c 3 e))
(every? number? '(1 2 3 4 5))



(define exists?
  (lambda (pred lst)
    (cond 
      ((null? lst) #f)
      ((pred (car lst)) #t)
      (else (exists? pred (cdr lst))))))
(exists? number? '(a b c d))
(exists? number? '(a b 1 d))



(define flatten
  (lambda (slist)
    (cond
      ((null? slist) '())
      ((symbol? slist) (list slist))
      (else (append (flatten (car slist)) (flatten (cdr slist)))))))
      
(flatten '(a b c))
(flatten '((a) () (b ()) () (c)))
(flatten '((a b) c (((d)) e)))
(flatten '(a b (() (c))))



(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (append (car lst) (up (cdr lst))))))

(up '((1 2) (3 4)))
(up '((x (y) z)))



(define merge
  (lambda (loi1 loi2)
    (cond ((null? loi1) loi2)
          ((null? loi2) loi1)
          ((< (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2)))
          (else (cons (car loi2) (merge loi1 (cdr loi2)))))))

(merge '(1 4) '(1 2 8))
(merge '(35 62 81 90 91) '(3 83 85 90))



(define sort
  (lambda (loi)
    (cond ((null? loi) '())
          ((null? (cdr loi)) loi)
          (else
           (let ((subs (split loi '()))) 
            (merge (sort (car subs)) (sort (cadr subs))))))))

(define split
  (lambda (lst accum)
    (if (<= (length lst) (length accum))
        (list lst accum)
        (split (cdr lst) (cons (car lst) accum)))))

(sort '(8 2 5 2 3))



(define sort/predicate
  (lambda (pred loi)
    (cond ((null? loi) '())
          ((null? (cdr loi)) loi)
          (else
           (let ((subs (split loi '()))) 
            (merge/predicate pred (sort/predicate pred (car subs)) (sort/predicate pred (cadr subs))))))))

(define merge/predicate
  (lambda (pred loi1 loi2)
    (cond ((null? loi1) loi2)
          ((null? loi2) loi1)
          ((pred (car loi1) (car loi2)) (cons (car loi1) (merge/predicate pred (cdr loi1) loi2)))
          (else (cons (car loi2) (merge/predicate pred loi1 (cdr loi2)))))))

(sort/predicate > '(8 2 5 2 3))
(sort/predicate < '(8 2 5 2 3))



(define leaf
  (lambda (n) n))

(define node
  (lambda (sym l r)
    (list sym l r)))

(define the-tree
  (node 'red
    (node 'b
      (node 'red 1 2)
      3)
    (node 'red 4 (node 'e 5 6))))

(define leaf?
  (lambda (n) (number? n)))

(define lson (lambda (node) (cadr node)))
(define rson (lambda (node) (caddr node)))
(define contents-of (lambda (node) (car node)))

(lson the-tree)
(rson the-tree)
(contents-of the-tree)



(define double
  (lambda (tree)
    (if (leaf? tree) 
        (* tree 2)
        (node (contents-of tree) (double (lson tree)) (double (rson tree))))))

(double the-tree)



(define mark-leaves-with-red-depth-from
  (lambda (tree n)
    (if (leaf? tree)
        n
        (let ((next-level (+ n (if (eq? 'red (contents-of tree)) 1 0))))
          (node (contents-of tree)
              (mark-leaves-with-red-depth-from (lson tree) next-level)
              (mark-leaves-with-red-depth-from (rson tree) next-level))))))

(define mark-leaves-with-red-depth
  (lambda (tree) (mark-leaves-with-red-depth-from tree 0)))

(mark-leaves-with-red-depth the-tree)



(define val (lambda (t) (car t)))
(define ltree (lambda (t) (cadr t)))
(define rtree (lambda (t) (caddr t)))

(define path
  (lambda (n tree)
    (cond ((eq? n (val tree)) '())
          ((< n (val tree)) (cons 'left (path n (ltree tree))))
          (else (cons 'right (path n (rtree tree)))))))                 

(path 17 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 ()()))))



(define number-leaves
  (lambda (tree) tree))

(define last
  (lambda (tree)
    (cond ((leaf? tree) tree)
          (last (rson tree)))))

(define number-leaves-from
  (lambda (tree n)
    (if (leaf? tree)
        n
        (let ((left (number-leaves-from (lson tree) n)))
          (node (contents-of tree) left (number-leaves-from (rson tree) (+ 1 (last left))))))))
             
(define number-leaves
  (lambda (tree)
    (number-leaves-from tree 0)))

(number-leaves the-tree)
(number-leaves (node 'foo
                     (node 'bar
                           (leaf 26)
                           (leaf 12))
                     (node 'baz
                           (leaf 11)
                           (node 'quux
                                 (leaf 117)
                                 (leaf 14)))))