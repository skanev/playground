(require rackunit rackunit/text-ui)
(load "../53.scm")

(define (first-value exp)
  (ambeval exp
           (setup-environment)
           (lambda (value fail) value)
           (lambda () '())))

(define sicp-4.53-tests
  (test-suite
    "Tests for SICP exercise 4.53"

    (check-equal?
      (first-value
        '(begin
           (define (prime-sum-pair list1 list2)
             (let ((a (an-element-of list1))
                   (b (an-element-of list2)))
               (require (prime? (+ a b)))
               (list a b)))
           (let ((pairs '()))
             (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
                        (permanent-set! pairs (cons p pairs))
                        (amb))
                      pairs))))
      '((8 35) (3 110) (3 20)))
))

(run-tests sicp-4.53-tests)
