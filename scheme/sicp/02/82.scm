; SICP exercise 2.82
;
; Show how the generalize apply-generic to handle coercion in the general case
; of multiple arguments. One strategy is to attempt to coerce all the
; arguments to the type of the first argument, then to the type of the second
; argument, and so on. Give an example of a situation where this strategy (and
; likewise the two-argument version given above) is not sufficiently general.
; (Hint: consider the case where there are some suitable mixed-type operations
; present in the table that will not be tried.)

; We want to test this. So first, we are going to implement all the necessary
; infrastructure. We shall not use real types, however, and we shall not
; introduce real operations. We would just have dummy operations and dummy
; conversions.

; Let's assume we have three types - a, b and c, where we have conversions
; a->b and b->c. Thus, c is the most generic type and a is the least generic
; one.

; Let's have the usual infrastructure. First, the table:

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

; Here's an intermediate version of apply-generic:

(define (apply-generic op . args)
  (define (id x) x)
  (define (get-coercion-or-id from to)
    (if (equal? from to)
        id
        (get-coercion from to)))
  (define (coerce-to args target-type)
    (let* ((type-tags (map type-tag args))
           (coercions (map (lambda (type) (get-coercion-or-id type target-type)) type-tags))
           (coerced-all? (not (memq #f coercions))))
      (if coerced-all?
        (map (lambda (coerce datum) (coerce datum)) coercions args)
        #f)))
  (define (find-coercion type-tags)
    (if (null? type-tags)
        #f
        (or (coerce-to args (car type-tags))
            (find-coercion (cdr type-tags)))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coerced-args (find-coercion type-tags)))
            (if coerced-args
              (apply apply-generic op coerced-args)
              (error "No method for these types" (list op type-tags))))))))

; Now, let's introduce our types:

(define (make-a) (attach-tag 'a "a"))
(define (make-b) (attach-tag 'b "b"))
(define (make-c) (attach-tag 'c "c"))

; Not some coercion operations:

(put-coercion 'a 'b (lambda (x) (attach-tag 'b (string-append (contents x) "->" "b"))))

; Our operations would be nonsensical - foo, bar and baz. 

(define (foo x y) (apply-generic 'foo x y))
(define (bar x y z) (apply-generic 'bar x y z))
(define (baz w x y z) (apply-generic 'baz w x y z))

(put 'foo '(a a) (lambda args (cons 'foo-a-a (map string->symbol args))))
(put 'foo '(b b) (lambda args (cons 'foo-b-b (map string->symbol args))))
(put 'bar '(a a a) (lambda args (cons 'bar-a-a-a (map string->symbol args))))
(put 'bar '(b b b) (lambda args (cons 'bar-b-b-b (map string->symbol args))))
(put 'baz '(a a a a) (lambda args (cons 'baz-a-a-a-a (map string->symbol args))))
(put 'baz '(b b b b) (lambda args (cons 'baz-b-b-b-b (map string->symbol args))))

; As for when this not sufficiently general - consider having foo defined for
; '(c c), but passing 'a and 'b - coercing just to b won't be enough. We need
; to coerce both arguments to c, but our apply-generic has no way of knowing
; that. Another example is if (foo b c) is defined and we call it with a and
; c. apply-generic will attempt to convert either to a or to c, but foo would
; not be defined for both.
