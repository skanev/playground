(require rackunit rackunit/text-ui)
(load "../51.scm")

(define (all-values exp)
  (ambeval exp
           (setup-environment)
           (lambda (value fail) (cons value (fail)))
           (lambda () '())))

(define sicp-4.51-tests
  (test-suite
    "Tests for SICP exercise 4.51"

    (check-equal? (all-values '(begin (define count 0)
                                      (let ((x (an-element-of '(a b c)))
                                            (y (an-element-of '(a b c))))
                                        (permanent-set! count (+ count 1))
                                        (require (not (eq? x y)))
                                        (list x y count))))
                  '((a b 2)
                    (a c 3)
                    (b a 4)
                    (b c 6)
                    (c a 7)
                    (c b 8)))
))

(run-tests sicp-4.51-tests)
