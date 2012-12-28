; SICP exercise 5.42
;
; Using find-variable from exercise 5.41, rewrite compile-variable and
; compile-assignment to output lexical address instructions. In cases where
; find-variable returns not-found (that is, where the variable is not in the
; compile-time environment), you should have the code generators use the
; evaluator operations, as before, to search for the binding. (The only place
; a variable that is not found at compile time can be is the global
; environment, which is part of the run-time environment but is not part of
; the compile-time environment. Thus, if you wish, you may have the evaluator
; operations look directly in the global environment, which can be obtained
; with the operation (op get-global-environment), instead of having them
; search the whole run-time environment found in env.) Test the modified
; compiler of a few simple cases, such as the nested lambda combination at the
; beginning of this section.

(load-relative "40.scm")
(load-relative "41.scm")

(define (compile-variable exp target linkage env)
  (let ((address (find-variable exp env)))
    (if (eq? address 'not-found)
        (end-with-linkage linkage
         (make-instruction-sequence '(env) (list target 'env)
          `((assign env (op the-global-environment))
            (assign ,target (op lookup-variable-value) (const ,exp) (reg env)))))
        (end-with-linkage linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target (op lexical-address-lookup) (const ,address) (reg env))))))))

(define (compile-assignment exp target linkage env)
  (let ((var (assignment-variable exp))
        (get-value-code (compile-exp (assignment-value exp) 'val 'next env)))
    (let ((address (find-variable var env)))
      (if (eq? address 'not-found)
          (end-with-linkage linkage
           (preserving '(env)
            get-value-code
            (make-instruction-sequence '(env val) (list target 'env)
             `((assign env (op the-global-environment))
               (perform (op set-variable-value!) (const ,var) (reg val) (reg env))
               (assign ,target (const ok))))))
          (end-with-linkage linkage
           (preserving '(env)
            get-value-code
            (make-instruction-sequence '(env val) (list target)
             `((perform (op lexical-address-set!) (const ,address) (reg val) (reg env))
               (assign ,target (const ok))))))))))
