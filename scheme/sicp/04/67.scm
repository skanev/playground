; SICP exercise 4.67
;
; Devise a way to install a loop detector in the query system so as to avoid
; the kinds of simple loops illustrated in the text and in exercise 4.64. The
; general idea is that the system should maintain some sort of history of its
; current chain of deductions and should not begin processing a query that it
; is already working on. Describe what kind of information (patterns and
; frames) is included in this history, and how the check should be made.
; (After you study the details of the query-system implementation in section
; 4.4.4, you may want to modify the system to include your loop detector).

; We need to maintain a history of each rule we applied. We can instantiate
; the rule with the current frame and store it (the result is a query where
; all the variables are free variables in the current frame). Every time we
; apply a rule, we check to see if we are not already processing the rule. We
; can do that by unifying the current rule with each one in the history. If
; they can be unified and all of the variables of the rule in the history
; remain free, then we are already processing the rule and we have detected a
; loop.
;
; The code below implements this. There are a bunch of new functions defined
; and the existing ones are modified so qeval takes an additional history
; argument. apply-a-rule is also modified to return the empty stream if it
; detects a loop.

(define loopy-rules
  '((married Minnie Mickey)
    (rule (married ?x ?y)
          (married ?y ?x))

    (rule (loopy-outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (loopy-outranked-by ?middle-manager ?boss)
                   (supervisor ?staff-person ?middle-manager))))))

; Loop Detection

(define (instantiate-pattern query frame)
  (define (rename-variable var)
    (if (number? (cadr var))
        (cons '? (cons (- (cadr var)) (cddr var)))
        (cons '? (cons '- (cdr var)))))
  (instantiate-exp query
                   frame
                   (lambda (v f) (rename-variable v))))

(define (free-var? var frame)
  (let ((binding (binding-in-frame var frame)))
    (or (not binding)
        (and (var? (binding-value binding))
             (free-var? (binding-value binding) frame)))))

(define (all-free? vars frame)
  (andmap (lambda (var) (free-var? var frame))
          vars))

(define (processing-query? query history)
  (ormap (lambda (history-entry) (same-query? history-entry query))
         history))

(define (variables query)
  (cond ((var? query)
         (list query))
        ((pair? query)
         (append (variables (car query))
                 (variables (cdr query))))
        (else '())))

(define (same-query? history-entry query)
  (let ((vars (variables history-entry))
        (unify-result (unify-match history-entry query '())))
    (and (not (eq? unify-result 'failed))
         (all-free? vars unify-result))))

; The Driver Loop and Instantiation

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query output:")
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
             (stream-map
               (lambda (frame)
                 (instantiate-exp q
                                  frame
                                  (lambda (v f) (contract-question-mark v))))
               (qeval q (singleton-stream '()) '())))
           (query-driver-loop)))))

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

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (display-stream stream)
  (unless (stream-empty? stream)
          (newline)
          (display (stream-first stream))
          (display-stream (stream-rest stream))))

; The Evaluator

(define (qeval query frame-stream history)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream history)
        (simple-query query frame-stream history))))

(define (execute exp)
  (apply (eval (predicate exp))
         (args exp)))

(define (simple-query query-pattern frame-stream history)
  (stream-flatmap
    (lambda (frame)
      (stream-append
        (find-assertions query-pattern frame)
        (apply-rules query-pattern frame history)))
    frame-stream))

(define (con-join conjuncts frame-stream history)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (con-join (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream
                      history)
               history)))

(define (dis-join disjuncts frame-stream history)
  (if (empty-disjunction? disjuncts)
      empty-stream
      (interleave (qeval (first-disjunct disjuncts) frame-stream history)
                  (dis-join (rest-disjuncts disjuncts)
                           frame-stream
                           history))))

(define (negate operands frame-stream history)
  (stream-flatmap
    (lambda (frame)
      (if (stream-empty? (qeval (negated-query operands) (singleton-stream frame) history))
          (singleton-stream frame)
          empty-stream))
    frame-stream))

(define (lisp-value call frame-stream history)
  (stream-flatmap
    (lambda (frame)
      (if (execute
            (instantiate-exp call
                             frame
                             (lambda (v f) (error "Unknown pat var -- LISP-VALUE" v))))
          (singleton-stream frame)
          empty-stream))
    frame-stream))

(define (always-true ignore frame-stream history)
  frame-stream)

; Finding Assertions by Pattern Matching

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum) (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        empty-stream
        (singleton-stream match-result))))

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

(define (apply-rules pattern frame history)
  (stream-flatmap (lambda (rule) (apply-a-rule rule pattern frame history))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame history)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (eq? unify-result 'failed)
          empty-stream
          (let ((current-query (instantiate-pattern (conclusion clean-rule) unify-result)))
            (if (processing-query? current-query history)
                empty-stream
                (qeval (rule-body clean-rule)
                       (singleton-stream unify-result)
                       (cons current-query history))))))))

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
  (reverse-list->stream THE-ASSERTIONS))
(define (get-indexed-assertions pattern)
  (reverse-list->stream
    (get-list (index-key-of pattern) 'assertion-list)))

(define THE-RULES '())
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules)
  (reverse-list->stream THE-RULES))
(define (get-indexed-rules pattern)
  (reverse-list->stream
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

  (put 'and 'qeval con-join)
  (put 'or 'qeval dis-join)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true))

(reset-state!)
