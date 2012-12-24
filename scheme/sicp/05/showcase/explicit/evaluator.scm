(load-relative "../simulator/simulator.scm")
(load-relative "controller-text.scm")
(load-relative "operations.scm")

(define (make-explicit-control-machine)
  (make-machine ec-registers ec-operations ec-controller-text))

(define (make-explicit-control-repl-machine)
  (make-machine ec-registers ec-operations ec-repl-controller-text))
