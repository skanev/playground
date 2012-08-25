; SICP exercise 2.83
;
; Suppose you are designing a generic artihmetic system for dealing with the
; tower of types shown in Figure 2.25: integer, rational, real and complex.
; For each type (except complex), design a procedure that raises objects of
; that type one level in the tower. Show how to install a generic raise
; operation that will work with each type (except complex).

; Ugh. We end up having tons of code again. Ok, let's start with the
; boilerplate. We shall use a simplified version of our previous code that
; does not support all the operations. Furthermore, we shall not hide stuff
; behind install-*-package.

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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types - APPLY-GENERIC" (list op type-tags))))))

; Now our types

(define (make-integer n) (attach-tag 'integer n))
(define (make-rational r d) (attach-tag 'rational (cons r d)))
(define (make-real x) (attach-tag 'real x))
(define (make-complex r i) (attach-tag 'complex (cons r i)))

(put 'raise '(integer) (lambda (n) (make-rational n 1)))
(put 'raise '(rational) (lambda (r) (make-real (* 1.0 (/ (car r) (cdr r))))))
(put 'raise '(real) (lambda (x) (make-complex x 0)))

(define (raise x)
  (apply-generic 'raise x))
