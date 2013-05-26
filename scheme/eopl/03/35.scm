; EOPL exercise 3.35
;
; Thre representation we have seen so far are inefficient, becuase they build
; a new closure every time the procedure is retrieved. But the closure is the
; same every time. We can build the closures only once, by putting the value
; in a vector of length 1 and building an explicit circular structure, like:
;
;   +------------+---+---+---+
;   | extend-env | . | . | o----> saved-env
;   +------------+-|-+-|-+---+
;          ^       |   |
;          |       V   V
;          |       x  +------------------------------------------------------------------------------------------+
;          |          |             +-----------+---+-----------------------------------------------------+---+  |
;          |          |   proc-val  | procedure | x | <<if zero?(x) then 0 else -((double -(x, 1)), -2)>> | . |  |
;          |          |             +-----------+---+-----------------------------------------------------+-|-+  |
;          |          +-------------------------------------------------------------------------------------|----+
;          |                                                                                                |
;          +------------------------------------------------------------------------------------------------+
;
; Here's the code to build the data structure.
;
;   (define extend-env-rec
;     (lambda (p-name b-var body saved-env)
;       (let ((vec (make-vector 1)))
;         (let ((new-env (extend-env p-name vec saved-env)))
;           (vector-set! vec 0 (proc-val (procedure b-var body new-env)))
;           new-env))))
;
; Complete the implementation of this representation by modifying the
; definitions of the environment data type and apply-env accordingly. Be sure
; that apply-env alwasy returns an expressed value.

(load-relative "cases/letrec/parser.scm")
(load-relative "cases/letrec/eval.scm")

(define environment? (or/c pair? null?))

(define (empty-env)
  '())

(define (extend-env var val env)
  (cons (list var val)
        env))

(define (extend-env* vars vals env)
  (if (null? vars)
      env
      (extend-env* (cdr vars)
                   (cdr vals)
                   (extend-env (car vars) (car vals) env))))

(define (extend-env-rec p-name b-var body saved-env)
  (let* ((vec (make-vector 1))
         (new-env (extend-env p-name vec saved-env)))
    (vector-set! vec 0 (proc-val (procedure b-var body new-env)))
    new-env))

(define (apply-env env var)
  (cond ((null? env) (eopl:error 'apply-env "Variable not found"))
        ((eqv? (caar env) var)
         (let ((val (cadar env)))
           (if (vector? val)
               (vector-ref val 0)
               val)))
        (#t (apply-env (cdr env) var))))
