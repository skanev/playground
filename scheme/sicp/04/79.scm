; SICP exercise 4.79
;
; When we implemented the Lisp evaluator in section 4.1, we saw how to use
; local environments to avoid name conflicts between the parameters of
; procedures. For example, in evaluating
;
; (define (square x)
;   (* x x))
; (define (sum-of-squares x y)
;   (+ (square x) (square y)))
; (sum-of-squares 3 4)
;
; there is no confusion between the x in square and the x in sum-of-squares,
; because we evaluate the body of each procedure in an environment that is
; specially constructed to contain bindings for the local variables. In the
; query system, we used a different strategy to avoid name conflicts in
; applying rules. Each time we apply a rule we rename the variables with new
; names that are guaranteed to be unique. The analogous strategy for the Lisp
; evaluator would be to do away with local environments and simply rename the
; variables in the body of a procedure each time we apply the procedure.
;
; Implement for the query language a rule-application method that uses
; environments rather than renaming. See if you can build on your environment
; structure to create constructs in the query language for dealing with large
; systems, such as the rule analog of block-structured procedures. Can you
; relate any of this to the problem of making deductions in a context (e.g.,
; "If I supposed that P were true, then I would be able to deduce A and B.")
; as a method of problem solving? (This problem is open-ended. A good answer
; is probably worth a Ph. D.).

; Phew. That was some seriously tricky business.
;
; Our environment would be a list of frame, where each frame is a list of
; bindings (a binding is an associative list, mapping variable to value). We
; will also have a special kind of variables, called "outer" variables. If a
; frame has a value for (? outer y), then this is the value of (? y) in the
; parent frame.
;
; We also need a procedure that "pops" an environment. It removes the top
; frame of the environment after copying all the outer variable assignments it
; has to its parents. That is, when the first frame in an environment binds
; (? outer y) to (1 2), then the procedure returns the second frame, extended
; with a binding form ?y to (1 2).
;
; The rule application is as follows:
;   1. We rename all variables in the query to outer variables
;   2. We unify the rule conclusion with the renamed query in a new
;      environment, whose parent is the current environment.
;   3. If the unification was successful, we proceed to evaluate the body of
;      the rule in the frame that resulted from the unification.
;   4. We map the resulting stream to the procedure that pops the enviornment
;
; This results to a stream of frames, that have bindings for all the
; variables in the query.
;
; In order to define rule-scoped rules and assertions, we are going to attach
; rules and assertions to each frame. We have a special (if awkward) syntax
; for inner definitions. Check out the tests for details.
;
; As for the implementation, there was a nice hint in the book. Namely, that
; fetch-rules and fetch-assertions take an extra environment argument. That
; way we can retrieve the rules or assertions in the current environment along
; with the ones in the database.
;
; For retrieving the rules, we take the rules of each frame and instantiate
; them in the frame (leaving the free variables as they are). We flatmap the
; result and remove duplicates to avoid checking a rule multiple times (this
; happens if a rule recursively invokes itself or the rule that defines it).
; This introduce an important semantic: the variables in the conclusion of the
; rule are bound within the inner rules. That is, a variable has the same
; value within the body of the rule and the bodies and conclusions of its
; inner rules.
;
; The process for assertions is the same.
;
; And I'm just going to ignore the open-ended question that gets you a Ph. D.

; Environments, Frames and Bindings

(define (empty-frame) (make-frame '() '() '() '()))
(define (make-frame bindings assertions rules parent)
  (list bindings assertions rules parent))
(define (frame-bindings frame) (car frame))
(define (frame-assertions frame) (cadr frame))
(define (frame-rules frame) (caddr frame))
(define (frame-parent frame) (cadddr frame))
(define (make-binding variable value) (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-env variable env)
  (assoc variable (frame-bindings env)))

; The selectors for rules

(define (rule? statement) (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-inner-assertions rule)
  (map cadr
       (filter (lambda (sexp) (eq? (car sexp) 'assert!))
               (cddr rule))))
(define (rule-inner-rules rule)
  (filter (lambda (sexp) (eq? (car sexp) 'rule))
          (cddr rule)))
(define (rule-body rule)
  (let ((stripped (filter (lambda (sexp)
                            (and (not (eq? (car sexp) 'rule))
                                 (not (eq? (car sexp) 'assert!))))
                          (cddr rule))))
    (if (null? stripped)
        '(always-true)
        (car stripped))))
(define (build-frame-for rule parent-env)
  (make-frame '()
              (rule-inner-assertions rule)
              (rule-inner-rules rule)
              parent-env))

; Helper procedures for "outer" variables

(define (outer? var)
  (and (not (null? (cddr var)))
       (eq? 'outer (cadr var))))

(define (remove-outer var)
  (cons '? (cddr var)))

(define (add-outer-to-variables pattern env)
  (instantiate-exp pattern
                   env
                   (lambda (var f) (append '(? outer) (cdr var)))))

; Helper procedures for environments

(define (substitute-variables expr env)
  (instantiate-exp expr env (lambda (var frame) var)))

(define (extend var val env)
  (make-frame (add-or-redefine var val (frame-bindings env))
              (frame-rules env)
              (frame-assertions env)
              (frame-parent env)))

(define (add-or-redefine var val bindings)
  (cons (make-binding var val)
        (filter (lambda (binding) (not (equal? (binding-variable binding) var)))
                bindings)))

(define (pop-env env)
  (define (merge binding frame)
    (let ((var (binding-variable binding))
          (val (binding-value binding)))
      (if (outer? var)
          (extend (remove-outer var)
                  (substitute-variables val env)
                  frame)
          frame)))
  (foldr merge (frame-parent env) (frame-bindings env)))

; Extracting rules and assertions from environments

(define (get-assertions-in-env env)
  (if (null? env)
      '()
      (append (map (lambda (assertion) (substitute-variables assertion env))
                   (frame-assertions env))
              (get-assertions-in-env (frame-parent env)))))

(define (get-rules-in-env env)
  (if (null? env)
      '()
      (append (map (lambda (rule) (substitute-variables rule env))
                   (frame-rules env))
              (get-rules-in-env (frame-parent env)))))

; Rule application

(define (apply-a-rule rule query-pattern query-env)
  (let ((new-env (build-frame-for rule query-env))
        (renamed-query (add-outer-to-variables query-pattern query-env)))
    (let ((unify-result (unify-match renamed-query
                                     (conclusion rule)
                                     new-env)))
      (if (eq? unify-result 'failed)
          empty-stream
          (stream-map
            pop-env
            (qeval (rule-body rule)
                   (singleton-stream unify-result)))))))

; The result of the interpreter:
; Instantiation

(define (instantiate-exp exp env unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-env exp env)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp env))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

; The Evaluator

(define (qeval query env-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) env-stream)
        (simple-query query env-stream))))

(define (execute exp)
  (apply (eval (predicate exp))
         (args exp)))

(define (simple-query query-pattern env-stream)
  (stream-flatmap
    (lambda (env)
      (stream-append
        (find-assertions query-pattern env)
        (apply-rules query-pattern env)))
    env-stream))

(define (conjoin conjuncts env-stream)
  (if (empty-conjunction? conjuncts)
      env-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      env-stream))))

(define (disjoin disjuncts env-stream)
  (if (empty-disjunction? disjuncts)
      empty-stream
      (interleave (qeval (first-disjunct disjuncts) env-stream)
                  (disjoin (rest-disjuncts disjuncts)
                           env-stream))))

(define (negate operands env-stream)
  (stream-flatmap
    (lambda (env)
      (if (stream-empty? (qeval (negated-query operands) (singleton-stream env)))
          (singleton-stream env)
          empty-stream))
    env-stream))

(define (lisp-value call env-stream)
  (stream-flatmap
    (lambda (env)
      (if (execute
            (instantiate-exp call
                             env
                             (lambda (v e) (error "Unknown pat var -- LISP-VALUE" v))))
          (singleton-stream env)
          empty-stream))
    env-stream))

(define (always-true ignore env-stream)
  env-stream)

; Finding Assertions by Pattern Matching

(define (find-assertions pattern env)
  (stream-flatmap (lambda (datum) (check-an-assertion datum pattern env))
                  (fetch-assertions pattern env)))

(define (check-an-assertion assertion query-pat query-env)
  (let ((match-result (pattern-match query-pat assertion query-env)))
    (if (eq? match-result 'failed)
        empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat env)
  (cond ((eq? env 'failed) 'failed)
        ((equal? pat dat) env)
        ((var? pat) (extend-if-consistent pat dat env))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       env)))
        (else 'failed)))

(define (extend-if-consistent var dat env)
  (let ((binding (binding-in-env var env)))
    (if binding
        (pattern-match (binding-value binding) dat env)
        (extend var dat env))))

; Rules and Unification

(define (apply-rules pattern env)
  (stream-flatmap (lambda (rule) (apply-a-rule rule pattern env))
                  (fetch-rules pattern env)))

(define (unify-match p1 p2 env)
  (cond ((eq? env 'failed) 'failed)
        ((equal? p1 p2) env)
        ((var? p1) (extend-if-possible p1 p2 env))
        ((var? p2) (extend-if-possible p2 p1 env))
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   env)))
        (else 'failed)))

(define (extend-if-possible var val env)
  (let ((binding (binding-in-env var env)))
    (cond (binding (unify-match (binding-value binding) val env))
          ((var? val)
           (let ((binding (binding-in-env val env)))
             (if binding
                 (unify-match var (binding-value binding) env)
                 (extend var val env))))
          ((depends-on? val var env) 'failed)
          (else (extend var val env)))))

(define (depends-on? exp var env)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-env e env)))
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
(define (fetch-assertions pattern env)
  (reverse-list->stream
    (remove-duplicates
      (append (get-assertions-in-env env)
              (if (use-index? pattern)
                (get-indexed-assertions pattern)
                (get-all-assertions))))))
(define (get-all-assertions)
  THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-list (index-key-of pattern) 'assertion-list))

(define THE-RULES '())
(define (fetch-rules pattern env)
  (reverse-list->stream
    (remove-duplicates
      (append (get-rules-in-env env)
              (if (use-index? pattern)
                (get-indexed-rules pattern)
                (get-all-rules))))))
(define (get-all-rules)
  THE-RULES)
(define (get-indexed-rules pattern)
  (append
    (get-list '? 'rule-list)
    (get-list (index-key-of pattern) 'rule-list)))

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

(define (list->stream items)
  (if (null? items)
      empty-stream
      (stream-cons (car items)
                   (list->stream (cdr items)))))

(define (reverse-list->stream items)
  (list->stream (reverse items)))

(define (get-list key1 key2)
  (let ((s (get key1 key2)))
    (if s s '())))

; Operator table

(define table (make-hash))

(define (put op type item)
  (hash-set! table (list op type) item))

(define (get op type)
  (hash-ref table (list op type) #f))

; Stream Operations

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons
        (stream-first s1)
        (interleave s2 (stream-rest s1)))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-empty? stream)
      empty-stream
      (interleave (stream-first stream)
                  (flatten-stream (stream-rest stream)))))

(define (singleton-stream x)
  (stream-cons x empty-stream))

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
