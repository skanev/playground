(require rackunit rackunit/text-ui)
(load "../81.scm")

(define (list->infinite-stream list)
  (define (next items)
    (if (null? items)
        (list->infinite-stream list)
        (stream-cons (car items) (next (cdr items)))))
  (next list))

(define sicp-3.81-tests
  (test-suite
    "Tests for SICP exercise 3.81"

    (check-equal? (stream-take
                    (random-numbers
                      (list->infinite-stream
                        '(reset 1 generate generate generate generate
                          reset 2 generate generate generate generate)))
                    8)
                  '(1015568748 1586005467 2165703038 3027450565
                    1017233273 1975575172 811535379 3186434646))
))

(run-tests sicp-3.81-tests)
