(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../15.scm")

(define sicp-5.15-tests
  (test-suite
    "Tests for SICP exercise 5.15"

    (test-begin "Counting instructions"
      (let ((machine (make-machine '(a) '()
                                   '(begin
                                       (goto (label middle))
                                       (assign a (const 1))
                                     middle
                                       (assign a (const 2))
                                       (assign a (const 3))
                                     end))))
        (start machine)
        (check-eq? (machine 'instruction-count) 3)
        (check-eq? (machine 'instruction-count) 0)))
))

(run-tests sicp-5.15-tests)
