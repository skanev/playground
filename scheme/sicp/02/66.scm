; SICP exercise 2.66
;
; Implement the lookup procedure for the case where the set of records is
; structured as a binary tree, ordered by the numerical values of the keys.

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let* ((record (entry set-of-records))
             (record-key (key record)))
        (cond ((= given-key record-key) record)
              ((< given-key record-key) (lookup given-key (left-branch set-of-records)))
              ((> given-key record-key) (lookup given-key (right-branch set-of-records)))))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (key record) (car record))
(define (name record) (cadr record))
