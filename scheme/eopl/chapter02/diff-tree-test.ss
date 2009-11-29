(module diff-tree-test scheme
  (require (planet schematics/schemeunit:3)
           (planet schematics/schemeunit:3/text-ui)
           "diff-tree.ss")

  (define (make-diff n)
    (let mk-diff ([a n] [b (zero)])
      (if (zero? a) b (mk-diff (sub1 a) (successor b)))))

  (define diff-tree-tests
    (test-suite
      "Diff tree integer representation"

      (check-equal? (one) '(one))
      (check-equal? (diff (one) (diff (one) (one))) '(diff (one) (diff (one) (one))))

      (check-equal? (diff->integer (zero)) 0)
      (check-equal? (diff->integer (diff (one) (diff (one) (one)))) 1)
      (check-equal? (diff->integer (diff (diff (one) (one)) (one))) -1)

      (check-equal? (zero) (diff (one) (one)))
      
      (check-pred is-zero? (zero))
      (check-pred is-zero? (diff (zero) (zero)))

      (check-equal? (successor (zero)) (diff (zero) (diff (zero) (one))))
      (check-equal? (diff->integer (successor (zero))) 1)

      (check-equal? (predecessor (one)) (diff (one) (one)))
      (check-equal? (diff->integer (predecessor (one))) 0)

      (check-equal? (diff->integer (make-diff 10)) 10)
      (check-equal? (diff->integer (predecessor (make-diff 10))) 9)

      (check-equal? (diff-plus (diff 'a 'b) (diff 'c 'd)) (diff (diff 'a 'b) (diff 'd 'c)))
      (check-equal? (diff->integer (diff-plus (make-diff 7) (make-diff 11))) 18)
      (check-equal? (diff->integer (diff-plus (make-diff 2) (one))) 3)
  ))

  (run-tests diff-tree-tests)
)
