(require rackunit rackunit/text-ui)
(load "../74.scm")

(define (list->infinite-stream list)
  (define (next items)
    (if (null? items)
        (list->infinite-stream list)
        (stream-cons (car items) (next (cdr items)))))
  (next list))

(define sense-data (list->infinite-stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))
(define zero-crossings '(0 0 0 0 0 -1 0 0 0 0 1 0 0))

(define sicp-3.74-tests
  (test-suite
    "Tests for SICP exercise 3.74"

    (check-equal? (stream-take (make-zero-crossings sense-data 0) 13)
                  zero-crossings)
    (check-equal? (stream-take (make-zero-crossings-with-map sense-data) 13)
                  zero-crossings)
))

(run-tests sicp-3.74-tests)
