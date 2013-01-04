(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../17.scm")

(define eopl-1.17-tests
  (test-suite
    "Tests for EOPL exercise 1.17"

    (check-equal? (down '(1 2 3))
                  '((1) (2) (3)))
    (check-equal? (down '((a) (fine) (idea)))
                  '(((a)) ((fine)) ((idea))))
    (check-equal? (down '(a (more (complicated)) object))
                  '((a) ((more (complicated))) (object)))
))

(exit (run-tests eopl-1.17-tests))
