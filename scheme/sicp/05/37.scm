; SICP exercise 5.37
;
; One way to understand the compiler's preserving mechanism for optimizing
; stack usage is to see what extra operations would be generated if we did not
; use this idea. Modify preserving so that it always generates the save and
; restore operations. Compile some simple expressions and identify the
; unnecessary stack operations that are generated. Compare the code to that
; generated with the preserving mechanism intact.

; For the simple expression

(define simple-if '(if true 1 0))

; There are 10 additional saves and restores:

(define annotated-unoptimized-code
  '(
      (save continue)         ; extra
      (save env)              ; extra
      (save continue)         ; extra
      (assign val (op lookup-variable-value) (const true) (reg env))
      (restore continue)      ; extra
      (restore env)           ; extra
      (restore continue)      ; extra
      (test (op false?) (reg val))
      (branch (label false-branch5))
    true-branch4
      (save continue)         ; extra
      (assign val (const 1))
      (restore continue)      ; extra
      (goto (reg continue))
    false-branch5
      (save continue)         ; extra
      (assign val (const 0))
      (restore continue)      ; extra
      (goto (reg continue))
    after-if6))

; In the original code there are no saves and restores. Here is how to
; generate it:

(load-relative "showcase/compiler/helpers.scm")

(define with-optimization (compiled-instructions simple-if))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (preserving (cdr regs)
         (make-instruction-sequence
          (list-union (list first-reg) (registers-needed seq1))
          (list-difference (registers-modified seq1) (list first-reg))
          (append `((save ,first-reg))
                  (statements seq1)
                  `((restore ,first-reg))))
         seq2))))

(define without-optimization (compiled-instructions simple-if))

(printf "With optimization:\n")
(pretty-print with-optimization)
(printf "\n\nWithout optimization:\n")
(pretty-print without-optimization)
