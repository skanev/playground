; SICP exercise 2.85
;
; This section mentioned a method for "simplifying" a data object by lowering
; it in the tower of types as far as possible. Design a procedure drop that
; accomplishes this for the tower described in Exercise 2.83. The key is to
; decide, in some general way, whether an object can be lowered. For example,
; the complex number 1.5 + 0i can be lowered as far as real, the complex
; number 1 + 0i can be lowered as far s integer, and the complex number 2 + 3i
; cannot be lowered at all. Here is a plan for determining whether an object
; can be lowered: begin by defining a generic operation project that "pushes"
; an object down in the tower. For example, projecting a complex number would
; involve throwing away the imaginary part. Then a number can be dropped if,
; when we project it and raise the result back to the type we started with, we
; end up with something equal to what we started with. Show how to implement
; this idea in detail, by writing a drop procedure that drops an object as far
; as possible. You will need to design the various projection operations and
; install project as a generic operation in the system. You will also need to
; make use of a generic equality predicate, such as described in exercise
; 2.79. Finally, use drop to rewrite apply-generic from exercise 2.84 so that
; it "simplifies" its answers.

; We base our work on the solution of the previous exercise. The
; implementation will be a bit flaky, since it is in no way the responsibility
; of apply-generic to simplify types. Instead, each operation should indicate
; whether it wants simplification. We shall just assume that every
; two-argument generic operation that returns a pair (i.e., tagged data) does
; simplificaiton. Otherwise, the result from drop and project would try to get
; simplified, which would lead to an infinite recursion. Furthermore, we shall
; call our procedure drop-down instead of drop, since the name drop is used in
; Racket and there are some weird rules going on.

; Again, we start with our usual infrastructure. First, the table:

(define table (make-hash))
(define (put op type item) (hash-set! table (list op type) item))
(define (get op type) (hash-ref table (list op type) #f))

; Next, the coercion operations:

(define (put-coercion from to op) (put 'coerce (list from to) op))
(define (get-coercion from to) (get 'coerce (list from to)))

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

(define (round-to-int x)
  (inexact->exact (truncate x)))

(define (make-integer n) (attach-tag 'integer n))
(define (make-rational n d)
  (let ((g (gcd n d)))
    (attach-tag 'rational (cons (round-to-int (/ n g))
                                (round-to-int (/ d g))))))
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
      (if proc
          (let ((result (apply proc (map contents args))))
            (if (and (= (length args) 2) (projectable? result))
                (drop-down result)
                result))
          (error "No method for these types - APPLY-GENERIC" (list op type-tags))))))

; Let's have an add operation we can use:

(put 'add '(integer integer) (lambda (x y) (make-integer (+ x y))))
(put 'add '(rational rational)
     (lambda (x y)
       (let* ((n1 (car x))
              (d1 (cdr x))
              (n2 (car y))
              (d2 (cdr y)))
         (make-rational (+ (* n1 d2) (* n2 d1))
                        (* d1 d2)))))
(put 'add '(real real) (lambda (x y) (make-real (+ x y))))
(put 'add '(complex complex)
     (lambda (z1 z2) (make-complex (+ (car z1) (car z2))
                                   (+ (cdr z1) (cdr z2)))))

(define (add x y) (apply-generic 'add x y))

; The equ? procedure:

(put 'equ? '(integer integer) =)
(put 'equ? '(rational rational) equal?)
(put 'equ? '(real real) =)
(put 'equ? '(complex complex) equal?)

(define (equ? x y) (apply-generic 'equ? x y))

; The project procedure:

(put 'project '(complex)
     (lambda (z) (make-real (car z))))
(put 'project '(real)
     (lambda (x) (make-rational (round-to-int (numerator x))
                                (round-to-int (denominator x)))))
(put 'project '(rational)
     (lambda (x) (make-integer (round-to-int (/ (car x) (cdr x))))))

(define (project x) (apply-generic 'project x))
(define (projectable? x) (and (pair? x) (get 'project (list (type-tag x)))))

; The drop procedure:

(define (drop-down x)
  (if (projectable? x)
      (let* ((projection (project x))
             (reraise (raise projection)))
        (if (equ? reraise x)
            (drop-down projection)
            x))
      x))

; Again, we do this for Racket scoping rules:
(define drop drop-down)
