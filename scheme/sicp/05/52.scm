; SICP exercise 5.52
;
; As a counterpoint to exercise 5.51, modify the compiler so that it compiles
; Scheme procedures into sequences of C instructions. Compile the metacircular
; evaluator of section 4.1 to produce a Scheme written in C.

; This exercise marks and end of a long journey. It uses an approach similar
; to the previous one. The code used to compile everything is in build.scm.
; There are modified versions of the metacircular evaluator and the compiler
; too (but noting noteworthy).
;
; Let's bring up some Racket-fu and use pattern matching.

(load-relative "support/52/metacircular-evaluator.scm")
(load-relative "support/52/compiler.scm")
(load-relative "support/52/syntax.scm")
(load-relative "support/52/build.scm")

; Some utility string transformation functions.

(define (dash-to-underscore str) (regexp-replace* (regexp "-") str "_"))
(define (question-mark-to-p str) (regexp-replace* (regexp "\\?$") str "_p"))
(define (exclamation-mark-to-bang str) (regexp-replace* (regexp "!") str "_bang"))

; Translates an instruction to a single line of C.

(define (translate inst)
  (match inst
    ; Assign
    [`(assign ,reg (op ,op) . ,args)
      (format "~a = ~a(~a);" (c-reg reg) (c-func-name op) (c-args args))]
    [`(assign ,reg ,val)
      (format "~a = ~a;" (c-reg reg) (c-val val))]

    ; Perform
    [`(perform (op ,op) . ,args)
      (format "~a(~a);" (c-func-name op) (c-args args))]

    ; Save
    [`(save ,reg)
      (format "push(~a);" (c-reg reg))]

    ; Restore
    [`(restore ,reg)
      (format "~a = pop();" (c-reg reg))]

    ; Test
    [`(test (op ,op) . ,args)
      (format "test = ~a(~a);" (c-func-name op) (c-args args))]

    ; Branch
    [`(branch (label ,label))
      (format "if (test) goto ~a;" (c-label-name label))]

    ; Goto
    [`(goto (label ,reg))
      (format "goto ~a;" (c-label-name reg))]
    [`(goto (reg ,reg))
      (format "goto *value_to_label(~a);" (c-reg reg))]

    ; Labels
    [(app symbol? #t)
     (format "~a:" (c-label-name inst))]

    [else (error "Unrecognized instruction:" inst)]))

; A bunch of utility procedures that know how to convert different fragments
; into the C code required.

(define (c-val val)
  (match val
    [`(reg ,reg) (c-reg reg)]
    [`(const ,const) (c-const const)]
    [`(label ,label) (format "label(&&~a)" (c-label-name label))]
    [else (error "Unknown c-val" val)]))

(define (c-const const)
  (cond ((symbol? const) (format "sym(\"~a\")" const))
        ((string? const) (format "str(~s)" const))
        ((number? const) (format "num(~a)" const))
        ((pair? const) (format "cons(~a, ~a)" (c-const (car const)) (c-const (cdr const))))
        ((null? const) (format "null()"))
        (else (error "Unknown c-const" const))))

(define (c-reg reg)
  (match reg
    ['continue "cont"]
    [else (symbol->string reg)]))

(define c-label-name
  (compose dash-to-underscore symbol->string))

(define c-func-name
  (compose exclamation-mark-to-bang
           dash-to-underscore
           question-mark-to-p
           symbol->string))

(define (c-args args)
  (string-join (map c-val args) ", "))

; Indents everything but the labels, so the compiled C code can look nicer.

(define (ident text)
  (if (regexp-match (regexp ":$") text)
      text
      (string-append "  " text)))

; Finally, the C code for the interpreter.

(define interpreter-in-c
  (let* ((instructions (compile-exp metacircular-evaluator 'val 'next))
         (statements (statements instructions))
         (lines (map (compose ident translate) statements)))
    (string-join lines "\n")))
