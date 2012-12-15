(require rackunit rackunit/text-ui)
(load "helpers/query.scm")
(load "../76.scm")

(define (env items)
  (if (null? items)
      '()
      (cons (cons (list '? (caar items))
                  (cadar items))
            (env (cdr items)))))

(define sicp-4.76-tests
  (test-suite
    "Tests for SICP exercise 4.76"

    (test-suite "merging frames"
      (check-equal? (merge-frames (env '((x 1) (y 2)))
                                  (env '((z 3) (x 1))))
                    (env '((y 2) (z 3) (x 1))))

      (check-equal? (merge-frames (env '((x (? y)) (z (1 2))))
                                  (env '((x (? z)) (y (1 (? two))))))
                    (env '((two 2) (z (1 (? two))) (x (? z)) (y (1 (? two)))))))

    (test-suite "simple queries"
      (check-equal? (matches-of '(and (job ?person (computer programmer))
                                      (address ?person ?where)))
                    '((and (job (Hacker Alyssa P) (computer programmer))
                           (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
                      (and (job (Fect Cy D) (computer programmer))
                           (address (Fect Cy D) (Cambridge (Ames Street) 3))))))

    (test-suite "rules"
      (check-true (matches? '(same x x)))

      (check-true (matches? '(lives-near (Hacker Alyssa P) (Fect Cy D))))
      (check-false (matches? '(lives-near (Hacker Alyssa P) (Bitdiddle Ben))))

      (check-true (matches? '(wheel (Warbucks Oliver))))
      (check-true (matches? '(wheel (Bitdiddle Ben))))
      (check-false (matches? '(wheel (Hacker Alyssa P)))))

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

(run-tests sicp-4.76-tests)
