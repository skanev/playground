(require rackunit rackunit/text-ui)
(load "../51.scm")

(define sicp-5.51-tests
  (test-suite
    "Tests for SICP exercise 5.51"

    (compile-interpreter)
    (check-equal? (interpreter-test-results) "(passed = 24 failed = 0)")
))

(run-tests sicp-5.51-tests)
