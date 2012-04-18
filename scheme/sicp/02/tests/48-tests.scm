(require rackunit rackunit/text-ui)
(load "../48.scm")

(define sicp-2.48-tests
  (test-suite
    "Tests for SICP exercise 2.48"

    (check-equal? (start-segment (make-segment (make-vect 1.0 2.0)
                                               (make-vect 3.0 4.0)))
                  (make-vect 1.0 2.0))

    (check-equal? (end-segment (make-segment (make-vect 1.0 2.0)
                                             (make-vect 3.0 4.0)))
                  (make-vect 3.0 4.0))
))

(run-tests sicp-2.48-tests)
