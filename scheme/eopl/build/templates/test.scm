(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "<%= config[:exercise_require_path] %>")

(define eopl-<%= config[:name] %>-tests
  (test-suite
    "Tests for EOPL exercise <%= config[:name] %>"

))

(exit (run-tests eopl-<%= config[:name] %>-tests))
