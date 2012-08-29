; SICP exercise 3.25
;
; Generalizing one- and two- dimensional tables, show how to implement a table
; in which values are stored under an arbitrary number of keys and different
; values may be stored under different numbers of keys. The lookup and insert!
; procedures should take as input a list of keys used to access the table.

; I fairly uncertain why we're not using list as key. In good spirit, however,
; I'm going to let that pass and implement nested tables.
;
(require r5rs/init)

(define (make-keyed-table key)
  (list key))

(define (make-table)
  (make-keyed-table '*table))

(define (find-pair key table)
  (define (search records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (search (cdr records)))))
  (search (cdr table)))

(define (lookup keys table)
  (let* ((first-key (car keys))
         (rest-keys (cdr keys))
         (record (find-pair first-key table)))
    (cond ((not record) #f)
          ((null? rest-keys) (cdr record))
          (else (lookup rest-keys record)))))

(define (insert! keys value table)
  (define (prepend-pair! pair)
    (set-cdr! table (cons pair (cdr table))))
  (let* ((first-key (car keys))
         (rest-keys (cdr keys))
         (pair (find-pair first-key table)))
    (cond ((and pair (null? rest-keys))
           (set-cdr! pair value))
          (pair
           (insert! rest-keys value pair))
          ((null? rest-keys)
           (prepend-pair! (cons first-key value)))
          (else
           (let ((new-table (make-keyed-table first-key)))
             (prepend-pair! new-table)
             (insert! rest-keys value new-table))))
    table))
