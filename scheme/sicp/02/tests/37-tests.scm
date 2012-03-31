(require rackunit rackunit/text-ui)
(load "../37.scm")

(define sicp-2.37-tests
  (test-suite
    "Tests for SICP exercise 2.37"

    (check-equal?
      (matrix-*-vector '((1 2)
                         (3 4))
                       '(5 6))
      '(17 39))

    (check-equal?
      (transpose '((1 2 3)
                   (4 5 6)
                   (7 8 9)))
      '((1 4 7)
        (2 5 8)
        (3 6 9)))

    (check-equal?
      (matrix-*-matrix '((1 2)
                         (3 4))
                       '((5 6)
                         (7 8)))
      '((19 22)
        (43 50)))
))

(run-tests sicp-2.37-tests)
