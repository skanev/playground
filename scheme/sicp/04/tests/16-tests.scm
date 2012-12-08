(require rackunit rackunit/text-ui)
(load "../16.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.16-tests
  (test-suite
    "Tests for SICP exercise 4.16"

    (check-exn (regexp "Unassigned variable x")
               (lambda () (run '(begin (define x '*unassigned*)
                                       x))))

    (check-equal? (scan-out-defines '((* x x)))
                  '((* x x)))
    (check-equal? (scan-out-defines '((define x 1)
                                      (+ x 2)))
                  '((let ((x '*unassigned*))
                     (set! x 1)
                     (+ x 2))))
    (check-equal? (scan-out-defines '((define x 1)
                                      (define y 2)
                                      (+ x y 3)))
                  '((let ((x '*unassigned*)
                          (y '*unassigned*))
                     (set! x 1)
                     (set! y 2)
                     (+ x y 3))))

    (check-equal? '(#t #f)
                  (run '(begin (define (f x)
                                 (list (even? x) (odd? x))
                                 (define (even? n)
                                   (if (= n 0)
                                       true
                                       (odd? (- n 1))))
                                 (define (odd? n)
                                   (if (= n 0)
                                       false
                                       (even? (- n 1)))))
                               (f 4))))
))

(run-tests sicp-4.16-tests)
