; SICP exercise 4.77
;
; In section 4.4.3 we saw that not and lisp-value can cause the query langauge
; to give "wrong" answers if these filtering operations are applied to frames
; in which variables are unbound. Devise a way to fix this shortcoming. One
; idea is to perform the filtering in a "delayed" manner by appending to the
; frame a "promise" to filter that is fulfilled only when enough variables
; have been bound to make the operation possible. We could wait to perform
; filtering until all other operations have been performed. However, for
; efficiency's sake, we would like to perform filtering as soon as possible so
; as to cut down on the number of intermediate frames generated.

; Sure. This is fun.
;
; We extend the frame to store an list of promises. Each promise is list of
; free variables and a predicate. Once all the free variables have been bound,
; the predicate is evaluted on the frame.
;
; This process is handled by the procedure compact-frame. It evaluates to
; topmost promise if all the necessary free variables have been bound. If the
; predicate fails, it returns 'failed. If it passes, it constructs a frame
; without the topmost promise and applies itself recursively. The process
; stops until (1) a promise with free variables is encountered, (2) all
; promises have been fulfiled or (3) a promise fails and compact-frame returns
; 'failed.
;
; compact-frame is invoked in extend after the new frame has been constructed.
; Since all the calls to extend take into account the possiblity of returing
; the symbol 'failed, there is system works like a charm.

; Frames, Bindings and Promises

(define (make-frame bindings promises)
  (list bindings promises))
(define (frame-bindings frame)
  (car frame))
(define (frame-promises frame)
  (cadr frame))
(define empty-frame (make-frame '() '()))

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding)
  (car binding))
(define (binding-value binding)
  (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable (frame-bindings frame)))
(define (extend variable value frame)
  (compact-frame
    (make-frame (cons (make-binding variable value)
                      (frame-bindings frame))
                (frame-promises frame))))

(define (make-promise variables predicate)
  (cons variables predicate))
(define (promise-variables promise)
  (car promise))
(define (promise-predicate promise)
  (cdr promise))

(define (free-var? var frame)
  (let ((binding (binding-in-frame var frame)))
    (or (not binding)
        (and (var? (binding-value binding))
             (free-var? (binding-value binding) frame)))))

(define (free-variables-in query frame)
  (cond ((null? query) '())
        ((and (var? query) (free-var? query frame)) (list query))
        ((pair? query) (append (free-variables-in (car query) frame)
                               (free-variables-in (cdr query) frame)))
        (else '())))

(define (compact-frame frame)
  (let ((promises (frame-promises frame)))
    (if (null? promises)
        frame
        (let* ((promise (car promises))
               (rest-promises (cdr promises))
               (vars (promise-variables promise))
               (predicate (promise-predicate promise))
               (has-free-vars? (ormap (lambda (var) (free-var? var frame)) vars)))
          (cond (has-free-vars? frame)
                ((predicate frame)
                 (compact-frame (make-frame (frame-bindings frame)
                                            rest-promises)))
                (else 'failed))))))

(define (promise-to-frame frame free-vars predicate)
  (make-frame (frame-bindings frame)
              (cons (make-promise free-vars predicate)
                    (frame-promises frame))))

; negate and lisp-value, modified to suit the new implementation

(define (negate operands frame-stream)
  (stream-map
    (lambda (frame)
      (promise-to-frame
        frame
        (free-variables-in (negated-query operands) frame)
        (lambda (frame)
          (stream-empty? (qeval (negated-query operands) (singleton-stream frame))))))
    frame-stream))

(define (lisp-value call frame-stream)
  (stream-map
    (lambda (frame)
      (promise-to-frame
        frame
        (free-variables-in call frame)
        (lambda (frame)
          (execute
            (instantiate-exp call
                             frame
                             (lambda (v f) (error "Unknown var -- LISP-VALUE" v)))))))
    frame-stream))

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
               (qeval q (singleton-stream '()))))
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

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (execute exp)
  (apply (eval (predicate exp))
         (args exp)))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append
        (find-assertions query-pattern frame)
        (apply-rules query-pattern frame)))
    frame-stream))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      empty-stream
      (interleave (qeval (first-disjunct disjuncts) frame-stream)
                  (disjoin (rest-disjuncts disjuncts)
                           frame-stream))))

(define (always-true ignore frame-stream)
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

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule) (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (eq? unify-result 'failed)
          empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

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
