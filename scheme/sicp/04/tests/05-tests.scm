(require rackunit rackunit/text-ui)
(load "../05.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.05-tests
  (test-suite
    "Tests for SICP exercise 4.05"

    (check-equal? (run '(begin (define (assoc key items)
                                 (cond ((null? items) false)
                                       ((eq? key (car (car items))) (car items))
                                       (else (assoc key (cdr items)))))
                               (define (cadr x) (car (cdr x)))
                               (cond ((assoc 'b '((a 1) (b 2))) => cadr)
                                     (else false))))
                  2)

    (check-equal? (run '(begin (define (assoc key items)
                                 (cond ((null? items) false)
                                       ((eq? key (car (car items))) (car items))
                                       (else (assoc key (cdr items)))))
                               (define (cadr x) (car (cdr x)))
                               (cond ((assoc 'c '((a 1) (b 2))) => cadr)
                                     (else 3))))
                  3)
))

(run-tests sicp-4.05-tests)
