; SICP exercise 2.92
;
; By imposing an ordering on variables, extend the polynomial package so that
; addition and multiplication of polynomials works for polynomials in
; different variables. (This is not easy!)

; That's gonna be fun!
;
; Let's use the code from the previous examples. We will choose sparse
; representation of polynomials. Furthermore, we shall only have one numerical
; type - scheme-number that we will dub number in order to simplify things.

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

(define (put-coercion from to op) (put 'coerce (list from to) op))
(define (get-coercion from to) (get 'coerce (list from to)))

; The type tower, supetype, supertype?, raise and project:

(define (supertype type)
  (get 'supertype type))

(define (supertype? a b)
  (let ((super (supertype a)))
    (cond ((equal? super b) #t)
          ((not super) #f)
          (else (supertype? super b)))))

(define (same-type? a b) (equal? (type-tag a) (type-tag b)))

(define (raise a) (apply-generic 'raise a))
(define (project a) (apply-generic 'project a))

(define (projectable? a) (get 'project (list (type-tag a))))
(define (raisable? a) (get 'raise (list (type-tag a))))

; Now the generic arithmemtic procedures.

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (neg x) (apply-generic 'neg x))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (square x) (mul x x))

; Numbers

(let ()
  (put 'add '(number number) +)
  (put 'sub '(number number) -)
  (put 'mul '(number number) *)
  (put 'div '(number number) /)
  (put 'neg '(number) -)
  (put 'equ? '(number number) =)
  (put '=zero? '(number) zero?))

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
              (let* ((new-c (div (coeff t1) (coeff t2)))
                     (new-o (- (order t1) (order t2)))
                     (term (make-term new-o new-c))
                     (multiplier (adjoin-term term (the-empty-termlist)))
                     (rest-of-result (div-terms (sub-terms L1 (mul-terms multiplier L2))
                                                L2))
                     (result-quotient (car rest-of-result))
                     (result-remainder (cadr rest-of-result)))
                (list (adjoin-term term result-quotient) result-remainder))))))
  (define (neg-poly p)
    (make-poly (variable p)
               (neg-terms (term-list p))))
  (define (add-poly p1 p2)
    (define (const var polynomial)
      (make-poly var (adjoin-term (make-term 0 (tag polynomial)) (the-empty-termlist))))
    (let ((v1 (variable p1))
          (v2 (variable p2))
          (t1 (term-list p1))
          (t2 (term-list p2)))
      (cond ((same-variable? v1 v2) (make-poly v1 (add-terms t1 t2)))
            ((before? v1 v2) (add-poly p1 (const v1 p2)))
            (else (add-poly (const v2 p1) p2)))))
  (define (mul-poly p1 p2)
    (define (const var polynomial)
      (make-poly var (adjoin-term (make-term 0 (tag polynomial)) (the-empty-termlist))))
    (let ((v1 (variable p1))
          (v2 (variable p2))
          (t1 (term-list p1))
          (t2 (term-list p2)))
      (cond ((same-variable? v1 v2) (make-poly v1 (mul-terms t1 t2)))
            ((before? v1 v2) (mul-poly p1 (const v1 p2)))
            (else (mul-poly (const v2 p1) p2)))))
  (define (sub-poly p1 p2)
    (define (const var polynomial)
      (make-poly var (adjoin-term (make-term 0 (tag polynomial)) (the-empty-termlist))))
    (let ((v1 (variable p1))
          (v2 (variable p2))
          (t1 (term-list p1))
          (t2 (term-list p2)))
      (cond ((same-variable? v1 v2) (make-poly v1 (sub-terms t1 t2)))
            ((before? v1 v2) (sub-poly p1 (const v1 p2)))
            (else (sub-poly (const v2 p1) p2)))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (map (lambda (term-list) (make-poly (variable p1) term-list))
           (div-terms (term-list p1)
                      (term-list p2)))
      (error "Polynomials not in same var - DIV-POLY" (list p1 p2))))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put '=zero? '(polynomial) (lambda (p) (empty-termlist? (term-list p))))
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))

  (put 'add '(number polynomial) (lambda (n p) (tag (add-poly (const (variable p) n) p))))
  (put 'mul '(number polynomial) (lambda (n p) (tag (mul-poly (const (variable p) n) p))))
  (put 'add '(polynomial number) (lambda (p n) (tag (add-poly p (const (variable p) n)))))
  (put 'mul '(polynomial number) (lambda (p n) (tag (mul-poly p (const (variable p) n)))))

  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms)))))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; apply-generic with some coercion:

(define (apply-generic op . args)
  (define (applicable? args)
    (get op (map type-tag args)))

  (define (apply-generic-failed)
    (error "No method for these types - APPLY-GENERIC" (list op (map type-tag args))))

  (define (all-of-same-type? args)
    (define (check rest)
      (cond ((null? rest) #t)
            ((same-type? (car args) (car rest)) (check (cdr rest)))
            (else #f)))
    (check args))

  (define (of-same-type-and-raisable? args)
    (and (all-of-same-type? args)
         (raisable? (car args))))

  (define (coercable-to-same-type? args)
    (and (= (length args) 2)
         (let ((type-a (type-tag (car args)))
               (type-b (type-tag (cadr args))))
           (or (supertype? type-a type-b)
               (supertype? type-b type-a)))))

  (define (coerce-to-same-type args)
    (and (= (length args) 2)
         (let* ((a (car args))
                (b (cadr args))
                (type-a (type-tag a))
                (type-b (type-tag b)))
           (cond ((same-type? a b) (list a b))
                 ((supertype? type-a type-b) (coerce-to-same-type (list (raise a) b)))
                 ((supertype? type-b type-a) (coerce-to-same-type (list a (raise b))))
                 (else #f)))))

  (define (attempt-coercion args)
    (let ((number-of-arguments (length args)))
      (cond ((of-same-type-and-raisable? args) (try (map raise args)))
            ((coercable-to-same-type? args) (try (coerce-to-same-type args)))
            (else (apply-generic-failed)))))

  (define (try args)
    (if (applicable? args)
        (apply (get op (map type-tag args)) (map contents args))
        (attempt-coercion args)))

  (try args))
