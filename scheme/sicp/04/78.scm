; SICP exercise 4.78
;
; Redesign the query langauge as a nondeterministic program to be implemented
; using the evaluator of section 4.3, rather than as a stream process. In this
; approach, each query will produce a single answer (rather than the stream of
; all answers) and the user can type try-again to see more answers. You should
; find that much of the mechanism we built in this section is subsumed by
; nondeterministic search and backtracking. You will probably also find,
; however, that your new query language has subtle differences in behavior
; from the one implemented here. Can you find examples that illustrate this
; difference?

; Let's not overdo it.
;
; I will implement this using an obscure PLaneT package that implements amb.
; Afterwards it will be straightforward enough to port this to the
; nondeterministic evaluator. I will, however, not do that yet. Maybe in the
; future.
;
; I would also cheat a bit by using amb-collect in negate.
;
; Furthermore, the all subtle differences have to do with either getting stuck
; in an infinite loop, having an infinite stream or the order of operations. I
; don't find that very interesting, so I will not elaborate.

(require (planet murphy/amb:1:1/amb))

; Some amb mumbo-jumbo

(define (require p)
  (unless p (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

; The Driver Loop and Instantiation

(define (instantiate-exp exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

; The Evaluator

(define (qeval query frame)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame)
        (simple-query query frame))))

(define (execute exp)
  (apply (eval (predicate exp))
         (args exp)))

(define (simple-query query-pattern frame)
  (amb (find-assertions query-pattern frame)
       (apply-rules query-pattern frame)))

(define (conjoin conjuncts frame)
  (if (empty-conjunction? conjuncts)
      frame
      (let ((match (qeval (first-conjunct conjuncts) frame)))
        (require (matched? match))
        (conjoin (rest-conjuncts conjuncts) match))))

(define (disjoin disjuncts frame)
  (if (empty-disjunction? disjuncts)
      (amb)
      (amb (qeval (first-disjunct disjuncts) frame)
           (disjoin (rest-disjuncts disjuncts) frame))))

(define (negate operands frame)
  (require (null? (amb-collect (qeval (negated-query operands) frame))))
  frame)

(define (lisp-value call frame)
  (require
    (execute
      (instantiate-exp call
                       frame
                       (lambda (v f) (error "Unknown pat var -- LISP-VALUE" v)))))
  frame)

(define (always-true ignore frame)
  frame)

; Finding Assertions by Pattern Matching

(define (failed? result)
  (eq? result 'failed))

(define (matched? result)
  (not (failed? result)))

(define (find-assertions pattern frame)
  (let ((assertion (an-element-of (fetch-assertions pattern frame))))
    (let ((match (pattern-match pattern assertion frame)))
      (require (matched? match))
      match)))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

; Rules and Unification

(define (apply-rules pattern frame)
  (let ((rule (an-element-of (fetch-rules pattern frame))))
    (let ((clean-rule (rename-variables-in rule)))
      (let ((unify-result (unify-match pattern
                                       (conclusion clean-rule)
                                       frame)))
        (require (matched? unify-result))
        (qeval (rule-body clean-rule) unify-result)))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding (unify-match (binding-value binding) val frame))
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame) 'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

; Maintaining the Data Base

(define THE-ASSERTIONS '())
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-all-assertions)
  (reverse THE-ASSERTIONS))
(define (get-indexed-assertions pattern)
  (reverse (get-list (index-key-of pattern) 'assertion-list)))

(define THE-RULES '())
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules)
  (reverse THE-RULES))
(define (get-indexed-rules pattern)
  (reverse
    (append
      (get-list '? 'rule-list)
      (get-list (index-key-of pattern) 'rule-list))))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS (cons assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (when (indexable? assertion)
        (let ((key (index-key-of assertion)))
          (let ((current-assertion-list (get-list key 'assertion-list)))
            (put key
                 'assertion-list
                 (cons assertion current-assertion-list))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (when (indexable? pattern)
          (let ((key (index-key-of pattern)))
            (let ((current-rule-list (get-list key 'rule-list)))
              (put key
                   'rule-list
                   (cons rule current-rule-list)))))))

(define (indexable? pattern)
  (or (constant-symbol? (car pattern))
      (var? (car pattern))))

(define (index-key-of pattern)
  (let ((key (car pattern)))
    (if (var? key) '? key)))

(define (use-index? pattern)
  (constant-symbol? (car pattern)))

(define (get-list key1 key2)
  (let ((s (get key1 key2)))
    (if s s '())))

; Operator table

(define table (make-hash))

(define (put op type item)
  (hash-set! table (list op type) item))

(define (get op type)
  (hash-ref table (list op type) #f))

; Query Syntax Procedures

(define (type exp)
  (if (pair? exp)
    (car exp)
    (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp) (eq? (type exp) 'assert!))
(define (add-assertion-body exp) (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement) (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule) (if (null? (cddr rule)) '(always-true) (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
                (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
    (string-append "?"
                   (if (number? (cadr variable))
                       (string-append (symbol->string (caddr variable))
                                      "-"
                                      (number->string (cadr variable)))
                       (symbol->string (cadr variable))))))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

; Frames and Bindings

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding)
  (car binding))
(define (binding-value binding)
  (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

; Reseting the state

(define (reset-state!)
  (set! table (make-hash))
  (set! rule-counter 0)
  (set! THE-ASSERTIONS '())
  (set! THE-RULES '())

  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true))

(reset-state!)
