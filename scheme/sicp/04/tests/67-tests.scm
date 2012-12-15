(require rackunit rackunit/text-ui)
(load "../67.scm")
(load-relative "../showcase/query/database.scm")

(define (matches-of query)
  (let ((processed-query (query-syntax-process query)))
    (stream->list
      (stream-map
        (lambda (frame)
          (instantiate-exp processed-query frame (lambda (v f) (contract-question-mark v))))
        (qeval processed-query (singleton-stream '()) '())))))

(define (matches? query)
  (not (null? (matches-of query))))

(define sicp-4.67-tests
  (test-suite
    "Tests for SICP exercise 4.67"

    (add-to-data-base! loopy-rules)

    (test-suite "loopy rules"
      (check-equal? (matches-of '(loopy-outranked-by (Bitdiddle Ben) ?who))
                    '((loopy-outranked-by (Bitdiddle Ben) (Warbucks Oliver))))
      (check-equal? (matches-of '(married Mickey ?who))
                    '((married Mickey Minnie))))

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
))

(run-tests sicp-4.67-tests)
