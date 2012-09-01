(require rackunit rackunit/text-ui)
(load "../50.scm")

(define sicp-3.50-tests
  (test-suite
    "Tests for SICP exercise 3.50"

    (check-equal?
      (stream->list (stream-map2 +
                                 (stream-cons 1 (stream-cons 2 (stream-cons 3 the-empty-stream)))
                                 (stream-cons 4 (stream-cons 5 (stream-cons 6 the-empty-stream)))))
      '(5 7 9))

    (check-equal?
      (stream->list (stream-map2 +
                                 (stream-cons 1 (stream-cons 2 (stream-cons 3 the-empty-stream)))
                                 (stream-cons 4 (stream-cons 5 (stream-cons 6 the-empty-stream)))
                                 (stream-cons 7 (stream-cons 8 (stream-cons 9 the-empty-stream)))))
      '(12 15 18))
))

(run-tests sicp-3.50-tests)
