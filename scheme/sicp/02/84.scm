; SICP exercise 2.84
;
; Using the raise operation in Exercise 2.83, modify the apply-generic
; procedure so that it coerces its arguments to have the same type by the
; method of succesive raising, as discussed in this section. You will need to
; devise a way to test which of two types is higher in the tower. Do this in a
; manner that is compatible with the rest of the system and will not lead to
; problems in adding new levels to the tower.

; Since we don't know about state yet, we have to accept some constraints.
; First, if new types can be added, they can make it only on the top or bottom
; of the type tower. Second, for simplicity, the operations supported by
; apply-generic will accept only two arguments. If we have that, we can
; implement the exercise nicely.

; Again, we start with our usual infrastructure. First, the table:

(define table (make-hash))

(define (put op type item)
  (hash-set! table (list op type) item))

(define (get op type)
  (hash-ref table (list op type) #f))

; Next, the coercion operations:

(define (put-coercion from to op)
  (put 'coerce (list from to) op))

(define (get-coercion from to)
  (get 'coerce (list from to)))

; Then, the type operations:

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum - TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum - CONTENTS" datum)))

; Now our types and the raise procedure.

(define (make-integer n) (attach-tag 'integer n))
(define (make-rational r d) (attach-tag 'rational (cons r d)))
(define (make-real x) (attach-tag 'real x))
(define (make-complex r i) (attach-tag 'complex (cons r i)))

(put 'raise '(integer) (lambda (n) (make-rational n 1)))
(put 'raise '(rational) (lambda (r) (make-real (* 1.0 (/ (car r) (cdr r))))))
(put 'raise '(real) (lambda (x) (make-complex x 0)))

(define (raise x) (apply-generic 'raise x))

; Now, we are going to implement a procedure that returns the supertype of
; another type.

(put 'supertype 'integer 'rational)
(put 'supertype 'rational 'real)
(put 'supertype 'real 'complex)

(define (supertype type)
  (get 'supertype type))

; Next, we implement a predicate that tells us whether one type is the
; supertype of another.

(define (supertype? parent child)
  (let ((type (supertype child)))
    (cond ((equal? type parent) #t)
          ((not type) #f)
          (else (supertype? parent type)))))

; Now we can define apply-generic:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc (apply proc (map contents args)))
            ((= (length type-tags) 2)
             (let ((type1 (car type-tags))
                   (type2 (cadr type-tags))
                   (x (car args))
                   (y (cadr args)))
               (cond ((supertype? type1 type2) (apply-generic op x (raise y)))
                     ((supertype? type2 type1) (apply-generic op (raise x) y))
                     (else (error "No method for these types - APPLY-GENERIC" (list op type-tags))))))
            (else (error "No method for these types - APPLY-GENERIC" (list op type-tags)))))))

; Finally, we have an operation we can test with:

(put 'foo '(integer integer) (lambda (x y) 'foo-integer))
(put 'foo '(rational rational) (lambda (x y) 'foo-rational))
(put 'foo '(real real) (lambda (x y) 'foo-real))
(put 'foo '(complex complex) (lambda (x y) 'foo-complex))

(define (foo x y) (apply-generic 'foo x y))

; Now if we need to add another supertype on the top or the bottom of the
; tower, we need to do two things - first, add it as a supertype of complex
; (if it is on the top) or add it as the supertype of integer (if it is on the
; bottom) and second, we need to provide a coercion procedure.
