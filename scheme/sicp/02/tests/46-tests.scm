(require rackunit rackunit/text-ui)
(load "../46.scm")

(define sicp-2.46-tests
  (test-suite
    "Tests for SICP exercise 2.46"

    (check-equal? 1.0
                  (xcor-vect (make-vect 1.0 2.0)))

    (check-equal? 2.0
                  (ycor-vect (make-vect 1.0 2.0)))

    (check-equal? (make-vect 4.0 6.0)
                  (add-vect (make-vect 1.0 2.0)
                            (make-vect 3.0 4.0)))

    (check-equal? (make-vect 3.0 1.0)
                  (sub-vect (make-vect 4.0 3.0)
                            (make-vect 1.0 2.0)))

    (check-equal? (make-vect 3.0 6.0)
                  (scale-vect 3.0
                              (make-vect 1.0 2.0)))
))

(run-tests sicp-2.46-tests)
