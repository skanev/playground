(require rackunit rackunit/text-ui)
(load "helpers/compiler.scm")
(load "../39.scm")

(define (env mappings)
  (if (null? mappings)
      the-empty-environment
      (extend-environment (caar mappings)
                          (cadar mappings)
                          (env (cdr mappings)))))

(define sicp-5.39-tests
  (test-suite
    "Tests for SICP exercise 5.39"

    (check-equal? (lexical-address-lookup '(0 0) (env '(((a b) (1 2)))))
                  1)

    (check-equal? (lexical-address-lookup '(0 1) (env '(((a b) (1 2)))))
                  2)

    (check-equal? (lexical-address-lookup '(1 0) (env '(((a b) (1 2))
                                                       ((c d) (3 4)))))
                  3)

    (check-equal? (lexical-address-lookup '(1 1) (env '(((a b) (1 2))
                                                       ((c d) (3 4)))))
                  4)

    (check-equal? (lexical-address-lookup '(2 0) (env '(((a b) (1 2))
                                                       ((c d) (3 4))
                                                       ((e f) (5 6)))))
                  5)

    (check-equal? (lexical-address-lookup '(2 1) (env '(((a b) (1 2))
                                                       ((c d) (3 4))
                                                       ((e f) (5 6)))))
                  6)

    (check-exn exn? (lambda () (lexical-address-lookup '(0 0) (env '(((a) (*unassigned*)))))))

    (test-case "setting a lexical variable in the top frame"
      (let ((environment (env '(((a b) (1 2))))))
        (lexical-address-set! '(0 1) 3 environment)
        (check-equal? (lexical-address-lookup '(0 1) environment) 3)))

    (test-case "setting a lexical variable in the top frame"
      (let ((environment (env '(((a b) (1 2))
                                ((c d) (3 4))))))
        (lexical-address-set! '(1 1) 5 environment)
        (check-equal? (lexical-address-lookup '(1 1) environment) 5)))
    ;(test-case "setting a lexical variable in a frame, other than the top")
))

(run-tests sicp-5.39-tests)
