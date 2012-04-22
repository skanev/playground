; SICP exercise 2.58
;
; Suppose we want to modify the differentiation program so that it works with
; ordinary mathematical notation, in which + and * are infix rather than prefix
; operators. Since the differentiation program is defined in terms of abstract
; data, we can modify it to work with different representations of expressions
; solely by changing the predicates, selectors and constructors that define the
; representation of algebraic expressions on which the differentiator is to
; operate.
;
; a. Show how to do this in order to differentiate algebraic expressions
;    presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify
;    the task, assume that + and * always take two arguments and that
;    expressions are fully parenthesized.
; b. The problem becomes substantially harder if we allow standard algebraic
;    notation, such as (x + 3 * (x + y + 2)), which drops unnecessary
;    parentheses and assumes that multiplication is done before addition. Can
;    you design appropriate predicates, selectors, and constructors for this
;    notation such that our derivative program still works?

; a. Sure enough. See code below
; b. Yup. I'm just going to sketch it (too lazy to write it down) - we need
;    to parse the sexp and put parentheses where they should be. Afterwards,
;    it is just as easy as calling a. Parsing the code is not that hard, but
;    I'm not in the mood for a parsing exercise at this point.

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr))))
        (else
          (error "unknown expression type - DERIV" expr))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x)
       (eq? (cadr x) '+)))

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x)
       (eq? (cadr x) '*)))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

(define (=number? expr num)
  (and (number? expr)
       (= expr num)))
