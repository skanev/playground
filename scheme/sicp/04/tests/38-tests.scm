(require rackunit rackunit/text-ui)
(load "../38.scm")

(define (all-values exp)
  (ambeval exp
           solution-environment
           (lambda (value fail) (cons value (fail)))
           (lambda () '())))

(define sicp-4.38-tests
  (test-suite
    "Tests for SICP exercise 4.38"

    (check-equal? (all-values '(multiple-dwelling))
                  '(((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
                    ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
                    ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
                    ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
                    ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))))
))

(run-tests sicp-4.38-tests)
