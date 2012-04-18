(require rackunit rackunit/text-ui)
(load "../47.scm")

(define sicp-2.47-tests
  (test-suite
    "Tests for SICP exercise 2.47"

    (check-equal? (origin-frame1 (make-frame1 1 2 3))
                  1)
    (check-equal? (edge1-frame1 (make-frame1 1 2 3))
                  2)
    (check-equal? (edge2-frame1 (make-frame1 1 2 3))
                  3)

    (check-equal? (origin-frame2 (make-frame2 1 2 3))
                  1)
    (check-equal? (edge1-frame2 (make-frame2 1 2 3))
                  2)
    (check-equal? (edge2-frame2 (make-frame2 1 2 3))
                  3)
))

(run-tests sicp-2.47-tests)
