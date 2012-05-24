; SICP exercise 2.73
;
; Section 2.3.2 described a program that performs symbolic differentiation:
;
; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp) (if (same-variable? exp var) 1 0))
;         ((sum? exp)
;          (make-sum (deriv (addend exp) var)
;                    (deriv (augend exp) var)))
;         ((product? exp)
;          (make-sum
;            (make-product (multiplier exp)
;                          (deriv (multiplicand exp) var))
;            (make-product (deriv (multiplicand exp) var)
;                          (multiplier exp))))
;         ; ...more rules can be added here
;         (else (error "unknown expression type - DERIV" exp))))
;
; We can regard this program as performing a dispatch on the type of the
; expression to be differentiated. In this situation the "type tag" of the
; datum is the algebraic operator symbol (such as +) and the operation being
; performed is deriv. We can transform this program into data-directed style by
; rewriting the basic derivative procedure as
;
; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp)
;          (if (same-variable? exp var) 1 0))
;         (else
;           ((get 'deriv (operator exp))
;            (operands exp)
;            var))))
;
; (define (operator exp) (car exp))
;
; (define (operands exp) (cdr exp))
;
; a. Explain what was done above. Why can't we assimilate the predicates
;    number? and variable? into the data-directed dispatch?
;
; b. Write the procedures for derivatives of sums and products, and the
;    auxiliary code required to install them in the table used by the program
;    above.
;
; c. Choose any additional differentiation rule that you like, such as the one
;    for exponents (exercise 2.56), and install it in this data-directed
;    system.
;
; d. In this simple algebraic manipulator the type of an expression is the
;    algebraic operator that binds it together. Suppose, however, we indexed
;    the procedures in the opposite way, so that the dispatch line in deriv
;    looked like
;
;    ((get (operator exp) 'deriv) (operands exp) var)
;
;    What corresponding changes to the derivative system are required?

; a. It's rather trivial what was done.
;
;    Anyway, we can't assimilate number? and variable? because they don't have
;    a type tag that can be indexed in the table.
;
; b. Check out below.
;
; c. Ditto.
;
; d. Simple. We just need to flip the op and type args to put.

; The whole shebang
(define (install-deriv-package)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (deriv-sum args var)
    (make-sum (deriv (addend args) var)
              (deriv (augend args) var)))
  (define (addend opers)
    (car opers))
  (define (augend opers)
    (cadr opers))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (deriv-product args var)
    (make-sum
      (make-product (multiplier args)
                    (deriv (multiplicand args) var))
      (make-product (deriv (multiplier args) var)
                    (multiplicand args))))
  (define (multiplier opers)
    (car opers))
  (define (multiplicand opers)
    (cadr opers))

  (define (make-exponentiation base power)
    (cond ((=number? power 0) 1)
          ((=number? power 1) base)
          (else (list '** base power))))
  (define (deriv-exponentiation args var)
    (make-product
      (make-product (power args)
                    (make-exponentiation (base args)
                                         (- (power args) 1)))
      (deriv (base args) var)))
  (define (base opers)
    (car opers))
  (define (power opers)
    (cadr opers))

  (define (=number? expr num)
    (and (number? expr)
         (= expr num)))

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation))



; The code you gave me
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        (else
          ((get 'deriv (operator expr))
           (operands expr)
           var))))

(define (operator expr)
  (car expr))

(define (operands expr)
  (cdr expr))



; The auxiliary stuff
(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))



; The table that we assumed is built-in. Don't peek.
(define table (make-hash))

(define (put op type item)
  (hash-set! table (list op type) item))

(define (get op type)
  (hash-ref table (list op type)))



(install-deriv-package)
