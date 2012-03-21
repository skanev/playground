(require rackunit rackunit/text-ui)
(load "../03.scm")

(define sicp-2.03-tests
  (test-suite
    "Tests for SICP exercise 2.03"

    (check-equal? (perimeter (make-rectangle (make-point 1 1)
                                             (make-point 2 2)))
                  4)
    (check-equal? (perimeter (make-rectangle (make-point 0 0)
                                             (make-point 3 4)))
                  14)

    (check-equal? (area (make-rectangle (make-point 1 1)
                                        (make-point 2 2)))
                  1)
    (check-equal? (area (make-rectangle (make-point 0 0)
                                        (make-point 3 4)))
                  12)
))

(run-tests sicp-2.03-tests)
