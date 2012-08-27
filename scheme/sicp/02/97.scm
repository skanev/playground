; SICP exercise 2.97
;
; a. Implement this algorithm as a procedure reduce-terms that takes two term
; lists n and d as arguments and returns a list nn, dd, which are n and d
; reduced to lowest terms via the algorithm given above. Also write a
; procedure reduce-poly, analogous to add-poly, that checks to see if the two
; polys have the same variable. If so, reduce-poly strips off the variable and
; passes the problem to reduce-terms, then reattaches the variable to the two
; term lists supplied by reduce-terms.
;
; b. Define a procedure analogous to reduce-terms that does what the original
; make-rat did for integers:
;
;   (define (reduce-integers n d)
;     (let ((g (gcd n d)))
;       (list (/ n g) (/ d g))))
;
; and define reduce as a generic operation that calls apply-generic to
; dispatch either reduce-poly (for polynomial arguments) or reduce-integers
; (for scheme-number arguments). You can now easily make the
; rational-arithmetic package reduce fractions to lowest terms by having
; make-rat call reduce before combining the given numberator and denominator
; to form a rational number. The system now handles rational expressions in
; either integers or polynomials.
;
; To test your program, try the example at the beginning of this extended
; exercise:
;
;   (define p1 (make-polynomial 'x '((1 1) (0 1))))
;   (define p2 (make-polynomial 'x '((3 1) (0 -1)))
;   (define p3 (make-polynomial 'x '((1 1))))
;   (define p4 (make-polynomial 'x '((2 1) (0 -1))))
;
;   (define rf1 (make-rational p1 p2))
;   (define rf2 (make-rational p3 p4))
;
;   (add rf1 rf2)
;
; See if you get the correct answer, correctly reduced to lowest terms.

; Our type system:

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum - TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum - CONTENTS" datum))))

(define table (make-hash))
(define (put op type item) (hash-set! table (list op type) item))
(define (get op type) (hash-ref table (list op type) #f))

; Now the generic arithmemtic procedures.

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (reduce x y) (apply-generic 'reduce x y))
(define (neg x) (apply-generic 'neg x))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (greatest-common-divisor x y) (apply-generic 'greatest-common-divisor x y))
(define (square x) (mul x x))

; Numbers

(let ()
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))

  (put 'add '(number number) +)
  (put 'sub '(number number) -)
  (put 'reduce '(number number) reduce-integers)
  (put 'mul '(number number) *)
  (put 'div '(number number) /)
  (put 'neg '(number) -)
  (put 'greatest-common-divisor '(number number) gcd)
  (put 'equ? '(number number) =)
  (put '=zero? '(number) zero?))

; Rationals:

(let ()
  (define (make-rat n d)
    (let ((terms (reduce n d)))
      (cons (car terms) (cadr terms))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (=zero?-rat x)
    (zero? (numer x)))

  (define (tag x) (attach-tag 'rational x))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put '=zero? '(rational) =zero?-rat)
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equal?)
  (put 'make 'rational (lambda (n d) (tag (make-rat n d)))))

(define (make-rational n d) ((get 'make 'rational) n d))
(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))

; A very clever procedure for normalizing terms. It is not nested under
; polynomials so it can be tested separately.

; The polynomial package:

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(let ()
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (before? v1 v2)
    (string<? (symbol->string v1) (symbol->string v2)))
  (define (variable? x) (symbol? x))

  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((null? term-list) (list term))
          ((> (order (car term-list)) (order term))
           (cons (car term-list) (adjoin-term term (cdr term-list))))
          ((= (order (car term-list)) (order term))
           (adjoin-term (make-term (order term)
                                   (add (coeff term) (coeff (car term-list))))
                        (cdr term-list)))
          (else (cons term term-list))))

  (define (const var number)
    (make-poly var (adjoin-term (make-term 0 number) (the-empty-termlist))))
  (define (poly-const var polynomial)
    (make-term var (adjoin-term (make-term 0 (tag polynomial)) (the-empty-termlist))))

  (define (map-terms proc L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((first (first-term L))
              (rest (rest-terms L)))
          (adjoin-term (make-term (order first) (proc (coeff first)))
                       (map-terms proc rest)))))
  (define (neg-terms L)
    (map-terms neg L))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1) (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1) (rest-terms L2)))))))))
  (define (sub-terms L1 L2)
    (add-terms L1 (neg-terms L2)))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                        (div-terms
                          (add-terms L1
                                     (neg-terms (mul-term-by-all-terms (make-term new-o new-c)
                                                                       L2)))
                          L2)))
                  (cons (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cdr rest-of-result))))))))
  (define (pseudoremainder-terms a b)
    (let ((factor (expt (coeff (car b))
                       (+ 1
                          (order (car a))
                          (- (order (car b)))))))
      (cadr (div-terms (mul-term-by-all-terms (make-term 0 factor) a) b))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (let ((divisor (apply gcd (map coeff a))))
          (map-terms (lambda (c) (/ c divisor)) a))
        (gcd-terms b (pseudoremainder-terms a b))))
  (define (reduce-terms n d)
    (let ((g (gcd-terms n d)))
      (list (car (div-terms n g))
            (car (div-terms d g)))))

  (define (neg-poly p)
    (make-poly (variable p)
               (neg-terms (term-list p))))
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (map (lambda (terms) (make-poly (variable p1) terms))
           (reduce-terms (term-list p1) (term-list p2)))
      (error "Polynomials not in same var - REDUCE-POLY" (list p1 p2))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polynomials not in same var - ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polynomials not in same var - MUL-POLY" (list p1 p2))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polynomials not in same var - SUB-POLY" (list p1 p2))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (map (lambda (term-list) (make-poly (variable p1) term-list))
             (div-terms (term-list p1)
                        (term-list p2)))
        (error "Polynomials not in same var - DIV-POLY" (list p1 p2))))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
        (error "Polynomials not in same var - GCD-POLY" (list p1 p2))))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put 'reduce '(polynomial polynomial) (lambda (p1 p2) (map tag (reduce-poly p1 p2))))
  (put '=zero? '(polynomial) (lambda (p) (empty-termlist? (term-list p))))
  (put 'greatest-common-divisor '(polynomial polynomial) (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))
  (put 'add '(number polynomial) (lambda (n p) (tag (add-poly (const (variable p) n) p))))
  (put 'mul '(number polynomial) (lambda (n p) (tag (mul-poly (const (variable p) n) p))))
  (put 'add '(polynomial number) (lambda (p n) (tag (add-poly p (const (variable p) n)))))
  (put 'mul '(polynomial number) (lambda (p n) (tag (mul-poly p (const (variable p) n)))))

  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms)))))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; apply-generic without any coercion:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types - APPLY-GENERIC" (list op type-tags))))))
