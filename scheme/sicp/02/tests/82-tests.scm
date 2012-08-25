(require rackunit rackunit/text-ui)
(load "../82.scm")

(define sicp-2.82-tests
  (test-suite
    "Tests for SICP exercise 2.82"

    (check-equal? (foo (make-a) (make-a)) '(foo-a-a a a))
    (check-equal? (foo (make-a) (make-b)) '(foo-b-b a->b b))
    (check-equal? (foo (make-b) (make-a)) '(foo-b-b b a->b))

    (check-equal? (bar (make-a) (make-a) (make-a)) '(bar-a-a-a a a a))
    (check-equal? (bar (make-a) (make-b) (make-b)) '(bar-b-b-b a->b b b))
    (check-equal? (bar (make-b) (make-a) (make-b)) '(bar-b-b-b b a->b b))
    (check-equal? (bar (make-b) (make-b) (make-a)) '(bar-b-b-b b b a->b))
    (check-equal? (bar (make-a) (make-a) (make-b)) '(bar-b-b-b a->b a->b b))
    (check-equal? (bar (make-a) (make-b) (make-a)) '(bar-b-b-b a->b b a->b))
    (check-equal? (bar (make-b) (make-a) (make-a)) '(bar-b-b-b b a->b a->b))

    (check-equal? (baz (make-a) (make-a) (make-a) (make-a)) '(baz-a-a-a-a a a a a))
    (check-equal? (baz (make-a) (make-b) (make-b) (make-b)) '(baz-b-b-b-b a->b b b b))
    (check-equal? (baz (make-b) (make-a) (make-b) (make-b)) '(baz-b-b-b-b b a->b b b))
    (check-equal? (baz (make-b) (make-b) (make-a) (make-b)) '(baz-b-b-b-b b b a->b b))
    (check-equal? (baz (make-b) (make-b) (make-b) (make-a)) '(baz-b-b-b-b b b b a->b))

    (check-exn exn? (lambda () (foo (make-a) (make-c))))
))

(run-tests sicp-2.82-tests)
