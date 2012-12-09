; SICP exercise 4.43
;
; Use the amb evaluator to solve the following puzzle:
;
;   Mary Ann Moore's father has a yacht and so has each of his four friends.
;   Colonel Downing, Mr. Hall, Sir Barnacle Hood, and Dr. Parker. Each of the
;   five also has one daughter and each has named his yacht after a daughter
;   of one of the others. Sir Barnacle's yacht is the Gabrielle, Mr. Moore
;   owns the Lorna; Mr. Hall the Rosalind. The Melissa, owned by Colonel
;   Downing, is named after Sir Barnacle's daughter. Gabrielle's father owns
;   the yacht that is named after Dr. Parker's daughter. Who is Lorna's
;   father?
;
; Try to write the program so that it runs efficiently (see exercise 4.40).
; Also determine how many solutions are there if we are not told that Mary
; Ann's last name is Moore.

(require r5rs/init)

; The solution to the puzzle is Colonel Downing.
;
; If we omit the fact that Mary Ann's last name is Moore, then there would be
; two solutions, the second of which is Dr. Parker.

(define solution
  '((define (map proc items)
      (if (null? items)
          '()
          (cons (proc (car items))
                (map proc (cdr items)))))
    (define (distinct? items)
      (cond ((null? items) true)
            ((null? (cdr items)) true)
            ((member (car items) (cdr items)) false)
            (else (distinct? (cdr items)))))
    (define (xor a b)
      (if a (not b) b))
    (define (yachts-and-daughters mary-ann-is-moore)
      (define (names) (amb 'gabrielle 'lorna 'rosalind 'mary-ann 'melissa))
      (define (daughter pair) (car pair))
      (define (yacht pair) (car (cdr pair)))
      (define (father-of girl fathers)
        (cond ((null? fathers) (error))
              ((eq? (daughter (car fathers)) girl) (car fathers))
              (else (father-of girl (cdr fathers)))))
      (define (daughter-and-yacht)
        (let ((daughter (names))
              (yacht (names)))
          (require (not (eq? daughter yacht)))
          (list daughter yacht)))
      (define (name-of-father daughter results)
        (if (eq? (car (cdr (car results))) daughter)
            (car (car results))
            (name-of-father daughter (cdr results))))
      (let ((moore (daughter-and-yacht)))
        (if mary-ann-is-moore
            (require (eq? (daughter moore) 'mary-ann))
            'ok)
        (require (eq? (yacht moore) 'lorna))
        (let ((barnacle (daughter-and-yacht)))
          (require (eq? (yacht barnacle) 'gabrielle))
          (require (eq? (daughter barnacle) 'melissa))
          (let ((hall (daughter-and-yacht)))
            (require (eq? (yacht hall) 'rosalind))
            (let ((downing (daughter-and-yacht)))
              (require (eq? (yacht downing) 'melissa))
              (let ((parker (daughter-and-yacht)))
                (let ((fathers (list moore barnacle hall downing parker)))
                  (require (distinct? (map yacht fathers)))
                  (require (distinct? (map daughter fathers)))
                  (require (eq? (daughter parker)
                                (yacht (father-of 'gabrielle fathers))))
                  (name-of-father 'lorna
                                  (list (cons 'moore moore)
                                        (cons 'barnacle barnacle)
                                        (cons 'hall hall)
                                        (cons 'downing downing)
                                        (cons 'parker parker))))))))))
    ))

; The rest of the interpreter.

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((amb? exp) (analyze-amb exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args) fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-value env)
                            (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                 (cproc env succeed fail2)
                 (aproc env succeed fail2)))
             fail))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application proc args succeed fail3))
                         fail2))
             fail))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3)
                                  (succeed (cons arg args) fail3))
                                fail2))
                    fail)))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (let? exp) (tagged-list? exp 'let))
(define (let->combination exp)
  (let ((names (map car (cadr exp)))
        (values (map cadr (cadr exp)))
        (body (cddr exp)))
    (cons (cons 'lambda (cons names body))
          values)))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last - COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable - SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'eq? eq?)
        (list 'null? null?)
        (list 'pair? pair?)
        (list 'list list)
        (list 'not not)
        (list 'abs abs)
        (list 'member member)
        (list 'display display)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '> >)
        (list '/ /)))

(define definitions
  '((define (require p)
      (if (not p) (amb)))
    (define (an-element-of items)
      (require (not (null? items)))
      (amb (car items) (an-element-of (cdr items))))))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (for-each (lambda (definition)
                (ambeval definition initial-env (lambda (value fail) 'ok) (lambda () 'ok)))
              definitions)
    initial-env))
(define the-global-environment (setup-environment))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (evaluate input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem")
            (ambeval input
                     the-global-environment
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     (lambda ()
                       (announce-output ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define solution-environment
  ((lambda ()
     (define environment (setup-environment))
     (for-each (lambda (definition)
                 (ambeval definition
                          environment
                          (lambda (value fail) 'ok)
                          (lambda () 'ok)))
               solution)
     environment)))
