(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../12.scm")

(define eopl-1.12-tests
  (test-suite
    "Tests for EOPL exercise 1.12"

    (check-equal? (subst 'z 'a '(a b a ((c a) d a (f a)) g a))
                  '(z b z ((c z) d z (f z)) g z))
))

(exit (run-tests eopl-1.12-tests))
