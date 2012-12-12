(require rackunit rackunit/text-ui)

(load "test-helpers.scm")

(define (stream . items)
  (if (null? items)
      empty-stream
      (stream-cons (car items) (apply stream (cdr items)))))

(define (env items)
  (if (null? items)
      '()
      (cons (cons (list '? (caar items))
                  (cadar items))
            (env (cdr items)))))

(define evaluator-tests
  (test-suite
    "Tests for the query language"

    (test-suite "simple queries"
      (check-equal? (matches-of '(job ?x (computer programmer)))
                    '((job (Hacker Alyssa P) (computer programmer))
                      (job (Fect Cy D) (computer programmer))))

      (check-equal? (matches-of '(job ?x (computer ?type)))
                    '((job (Bitdiddle Ben) (computer wizard))
                      (job (Hacker Alyssa P) (computer programmer))
                      (job (Fect Cy D) (computer programmer))
                      (job (Tweakit Lem E) (computer technician))))

      (check-equal? (matches-of '(job ?x (computer . ?type)))
                    '((job (Bitdiddle Ben) (computer wizard))
                      (job (Hacker Alyssa P) (computer programmer))
                      (job (Fect Cy D) (computer programmer))
                      (job (Tweakit Lem E) (computer technician))
                      (job (Reasoner Louis) (computer programmer trainee))))

      (check-equal? (matches-of '(and (job ?person (computer programmer))
                                      (address ?person ?where)))
                    '((and (job (Hacker Alyssa P) (computer programmer))
                           (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
                      (and (job (Fect Cy D) (computer programmer))
                           (address (Fect Cy D) (Cambridge (Ames Street) 3)))))

      (check-equal? (matches-of '(or (supervisor ?x (Bitdiddle Ben))
                                     (supervisor ?x (Hacker Alyssa P))))
                    '((or (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
                          (supervisor (Hacker Alyssa P) (Hacker Alyssa P)))
                      (or (supervisor (Reasoner Louis) (Bitdiddle Ben))
                          (supervisor (Reasoner Louis) (Hacker Alyssa P)))
                      (or (supervisor (Fect Cy D) (Bitdiddle Ben))
                          (supervisor (Fect Cy D) (Hacker Alyssa P)))
                      (or (supervisor (Tweakit Lem E) (Bitdiddle Ben))
                          (supervisor (Tweakit Lem E) (Hacker Alyssa P)))))

      (check-equal? (matches-of '(and (supervisor ?x (Bitdiddle Ben))
                                      (not (job ?x (computer programmer)))))
                    '((and (supervisor (Tweakit Lem E) (Bitdiddle Ben))
                           (not (job (Tweakit Lem E) (computer programmer))))))

      (check-equal? (matches-of '(and (salary ?person ?amount)
                                      (lisp-value > ?amount 30000)))
                    '((and (salary (Warbucks Oliver) 150000)
                           (lisp-value > 150000 30000))
                      (and (salary (Bitdiddle Ben) 60000)
                           (lisp-value > 60000 30000))
                      (and (salary (Hacker Alyssa P) 40000)
                           (lisp-value > 40000 30000))
                      (and (salary (Fect Cy D) 35000)
                           (lisp-value > 35000 30000))
                      (and (salary (Scrooge Eben) 75000)
                           (lisp-value > 75000 30000)))))

    (test-suite "rules"
      (check-true (matches? '(same x x)))

      (check-true (matches? '(lives-near (Hacker Alyssa P) (Fect Cy D))))
      (check-false (matches? '(lives-near (Hacker Alyssa P) (Bitdiddle Ben))))

      (check-true (matches? '(wheel (Warbucks Oliver))))
      (check-true (matches? '(wheel (Bitdiddle Ben))))
      (check-false (matches? '(wheel (Hacker Alyssa P))))

      (check-true (matches? '(outranked-by (Bitdiddle Ben) (Warbucks Oliver))))
      (check-true (matches? '(outranked-by (Hacker Alyssa P) (Warbucks Oliver))))
      (check-true (matches? '(outranked-by (Reasoner Louis) (Warbucks Oliver))))
      (check-true (matches? '(outranked-by (Hacker Alyssa P) (Bitdiddle Ben))))
      (check-true (matches? '(outranked-by (Reasoner Louis) (Bitdiddle Ben))))
      (check-true (matches? '(outranked-by (Reasoner Louis) (Hacker Alyssa P))))

      (check-false (matches? '(outranked-by (Warbucks Oliver) (Bitdiddle Ben))))
      (check-false (matches? '(outranked-by (Eben Scrooge) (Bitdiddle Ben))))
      (check-false (matches? '(outranked-by (Bitdiddle Ben) (Eben Scrooge)))))

    (test-suite "logic as programs"
      (check-equal? (matches-of '(append-to-form (a b) (c d) ?z))
                    '((append-to-form (a b) (c d) (a b c d))))
      (check-equal? (matches-of '(append-to-form (a b) ?y (a b c d)))
                    '((append-to-form (a b) (c d) (a b c d))))
      (check-equal? (matches-of '(append-to-form ?x ?y (a b c d)))
                    '((append-to-form () (a b c d) (a b c d))
                      (append-to-form (a) (b c d) (a b c d))
                      (append-to-form (a b) (c d) (a b c d))
                      (append-to-form (a b c) (d) (a b c d))
                      (append-to-form (a b c d) () (a b c d)))))

    (test-suite "always true"
      (check-equal? (stream->list (qeval '(always-true) (singleton-stream '())))
                    '(())))

    (test-suite "pattern matching"
      (check-equal? (pattern-match 'a 'a '()) '())
      (check-equal? (pattern-match 'a 'b '()) 'failed)
      (check-equal? (pattern-match '(? x) 'a '()) (env '((x a))))

      (check-equal?
        (pattern-match '(same (? x) (? x)) '(same a a) '())
        (env '((x a))))

      (check-equal?
        (pattern-match '(same (? x) (? x)) '(same a b) '())
        'failed)

      (check-equal?
        (pattern-match '(same (? x) a) '(same a a) (env '((x a))))
        (env '((x a))))

      (check-equal?
        (pattern-match '(? x) '(f b) (env '((x (f (? y))))))
        (env '((y b) (x (f (? y))))))

      (check-equal?
        (pattern-match '(? x) '(f b) (env '((x (f (? y))) (y b))))
        (env '((x (f (? y))) (y b))))

      (check-equal?
        (pattern-match '(? x) '(f b) (env '((x (f (? y))) (y c))))
        'failed))

    (test-suite "rename variables"
      (before (reset-state!)
        (check-equal?
          (rename-variables-in '(same (? x) (? y)))
          '(same (? 1 x) (? 1 y)))))

    (test-suite "depends-on?"
      (check-false (depends-on? '(same (? x)) '(? y) '()))
      (check-true (depends-on? '(same (? x)) '(? x) '()))
      (check-true (depends-on? '(same (? x)) '(? y) (env '((x (? y)))))))

    (test-suite "unification"
      (check-equal?
        (unify-match 'a 'a '())
        '())

      (check-equal?
        (unify-match 'a 'b '())
        'failed)

      (check-equal?
        (unify-match '(? x) '(? y) '())
        (env '((x (? y)))))

      (check-equal?
        (unify-match '(same (? x) b) '(same a (? y)) '())
        (env '((y b) (x a))))

      (check-equal?
        (unify-match '((? x) (? x)) '((? y) (? y)) '())
        (env '((x (? y)))))

      (check-equal?
        (unify-match '(same (person (? a)) (? b)) '(same (? c) (? c)) (env '((b (person smith)))))
        (env '((a smith)
               (c (person (? a)))
               (b (person smith))))))

    (test-suite "adding to the database"
      (test-case "adding an assertion"
        (reset-state!)

        (add-assertion! '(father luke anakin))

        (check-equal? (stream->list (get-all-assertions))
                      '((father luke anakin))))

      (test-case "indexing assertions"
        (reset-state!)

        (add-assertion! '(father luke anakin))
        (add-assertion! '(son anakin luke))

        (check-equal? (stream->list (fetch-assertions '(son (? x) (? x)) '()))
                      '((son anakin luke)))

        (check-equal? (stream->list (fetch-assertions '(father (? x) (? x)) '()))
                      '((father luke anakin))))

      (test-case "adding a rule"
        (reset-state!)

        (add-rule! '(rule (same (? x) (? x))))

        (check-equal? (stream->list (get-all-rules))
                      '((rule (same (? x) (? x))))))

      (test-case "indexing rules"
        (reset-state!)

        (add-rule! '(rule (same (? x) (? x))))
        (add-rule! '(rule (two (? x) (? y))))
        (add-rule! '(rule ((? x) might-be (? y))))

        (check-equal? (stream->list (fetch-rules '(same a a) '()))
                      '((rule (same (? x) (? x)))
                        (rule ((? x) might-be (? y)))))

        (check-equal? (stream->list (fetch-rules '((? x) a a) '()))
                      '((rule (same (? x) (? x)))
                        (rule (two (? x) (? y)))
                        (rule ((? x) might-be (? y)))))))

    (test-suite "query-syntax-process"
      (check-equal? (query-syntax-process '?a) '(? a))
      (check-equal? (query-syntax-process '(?a ?b)) '((? a) (? b)))
      (check-equal? (query-syntax-process '(a (b ?c) ?d)) '(a (b (? c)) (? d))))

    (test-suite "make-new-variable"
      (check-equal? (make-new-variable '(? x) 1) '(? 1 x)))

    (test-suite "contract-question-mark"
      (check-equal? (contract-question-mark '(? x)) '?x)
      (check-equal? (contract-question-mark '(? 1 x)) '?x-1))

    (test-suite "stream operations"
      (check-equal? (stream->list (interleave (stream 1 3 5 7 9) (stream 2 4 6 8 10)))
                    '(1 2 3 4 5 6 7 8 9 10))
      (check-equal? (stream->list (stream-flatmap (lambda (n) (stream (* n 10) (+ (* n 10) 5)))
                                                  (stream 1 2 3 4 5)))
                    '(10 20 15 30 25 40 35 50 45 55))
      (check-equal? (stream->list (singleton-stream 1))
                    '(1)))
))

(run-tests evaluator-tests)
