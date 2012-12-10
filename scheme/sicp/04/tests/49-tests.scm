(require rackunit rackunit/text-ui)
(load "../49.scm")

(define (first-n-values n exp)
  (define (take n results)
    (if (= n 0)
        '()
        (cons (car results)
              (take (- n 1) (force (cdr results))))))
  (take n
        (ambeval exp
                 solution-environment
                 (lambda (value fail) (cons value (delay (fail))))
                 (lambda () '()))))


(define sicp-4.49-tests
  (test-suite
    "Tests for SICP exercise 4.49"

    (check-equal? (first-n-values 6 '(terminals (generate-sentence)))
                  '((the student studies)
                    (the student studies for the student)
                    (the student studies for the student for the student)
                    (the student studies for the student for the student for the student)
                    (the student studies for the student for the student for the student for the student)
                    (the student studies for the student for the student for the student for the student for the student)))
))

(run-tests sicp-4.49-tests)
