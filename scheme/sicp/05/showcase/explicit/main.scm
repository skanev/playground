(load-relative "evaluator.scm")

(define ec-repl-machine (make-explicit-control-repl-machine))
(start ec-repl-machine)
