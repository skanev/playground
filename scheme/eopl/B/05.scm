; EOPL exercise B.05
;
; Add unary minus to the language and interpreter, so that inputs like 3*-2
; are handled correctly.

(define scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (additive-op ((or "+" "-")) symbol)
    (multiplicative-op ((or "*" "/")) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((expression
      (term (arbno additive-op term))
      op)
    (term
      (factor (arbno multiplicative-op factor))
      op)
    (factor
      (number)
      number)
    (factor
      (identifier)
      ref)
    (factor
      ("-" factor)
      neg)
    (factor
      ("(" expression ")")
      factor)))

(define-datatype ast ast?
  (op
    (first-operand ast?)
    (operators (list-of symbol?))
    (rest-operands (list-of ast?)))
  (factor
    (value ast?))
  (neg
    (value ast?))
  (ref
    (name symbol?))
  (number
    (value integer?)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

(define (eval* tree env)
  (cases ast tree
    (op (first ops rest)
      (apply-ops (eval* first env)
                 ops
                 (map (curryr eval* env) rest)))
    (number (val)
      val)
    (ref (var)
      (lookup var env))
    (neg (expr)
      (- (eval* expr env)))
    (factor (expr)
      (eval* expr env))))

(define (apply-ops first ops rest)
  (if (null? ops)
      first
      (apply-ops ((eval (car ops)) first (car rest))
                 (cdr ops)
                 (cdr rest))))

(define (lookup var env)
  (cond ((null? env)
         (eopl:error 'lookup "Variable not found: ~s" var))
        ((eqv? var (caar env))
         (cadar env))
        (else
         (lookup var (cdr env)))))

(define (value-of code env)
  (eval* (scan&parse code) env))
