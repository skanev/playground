(require rackunit rackunit/text-ui)
(load "../52.scm")

(define (first-value exp)
  (ambeval exp
           (setup-environment)
           (lambda (value fail) value)
           (lambda () '())))

(define sicp-4.52-tests
  (test-suite
    "Tests for SICP exercise 4.52"

    (check-equal? (first-value '(if-fail (let ((x (an-element-of '(1 3 5))))
                                           (require (even? x))
                                           x)
                                         'all-odd))
                  'all-odd)
    (check-equal? (first-value '(if-fail (let ((x (an-element-of '(1 3 5 8))))
                                           (require (even? x))
                                           x)
                                         'all-odd))
                  8)
))

(run-tests sicp-4.52-tests)
