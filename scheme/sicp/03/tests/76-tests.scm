(require rackunit rackunit/text-ui)
(load "../76.scm")

(define (list->infinite-stream list)
  (define (next items)
    (if (null? items)
        (list->infinite-stream list)
        (stream-cons (car items) (next (cdr items)))))
  (next list))

(define sense-data (list->infinite-stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define sicp-3.76-tests
  (test-suite
    "Tests for SICP exercise 3.76"

    (check-equal? (stream-take (smooth sense-data) 13)
                  '(1 1.5 1.25 1.0 0.75 0.45 -0.5 -1.0 -0.5 0.25 0.6 2.0 2.5))
    (check-equal? (stream-take (make-zero-crossings sense-data 0) 13)
                  '(0 0 0 0 0 0 -1 0 0 1 0 0 0))
))

(run-tests sicp-3.76-tests)
