; SICP exercise 5.08
;
; The following register-machine code is ambiguous, because the label here is
; defined more than once:
;
; start
;   (goto (label here))
; here
;   (assign a (const 3))
;   (goto (label there))
; here
;   (assign a (const 4))
;   (goto (label there))
; there
;
; With the simulator as written, what will the contents of register a be when
; control reaches there? Modify the extract-labels procedure so that the
; assembler will signal an error if the same label name is used to indicate
; two different locations.

; The result will be 3. Since the associative list of labels retains the order
; in which they are defined, goto will jump to the first label.
;
; Here's the modification:

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
        (lambda (insts labels)
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (if (assoc next-inst labels)
                    (error "Duplicate label:" next-inst)
                    (receive insts
                             (cons (make-label-entry next-inst insts)
                                   labels)))
                (receive (cons (make-instruction next-inst)
                               insts)
                         labels)))))))
