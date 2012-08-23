; SICP exercise 2.74
;
; Insatiable Enterprises, Inc. is a highly decentralized conglomerate
; consisting of a large number of independent divisions located all over the
; world. The company's computer facilities have just been interconnected by
; means of a clever network-interfacing scheme that makes the entire network
; appear to any user to be a single computer. Insatiable's president, in her
; first attempt to exploit the ability of the network to extract
; administrative information from division files, is dismayed to discover
; that, alhough all the division files have been implemeted as data structures
; in Scheme, the particular data structure used varies from division to
; division. A meeting of division managers is hastily called to search for a
; strategy to integrate the files that will satisfy headquarters' needs while
; preserving the existing autonomy of the divisions.
;
; Show how such a strategy can be implemeted with data-directed programming.
; As an example, suppose that each division personnel records consist of a
; single file, which contains a set of records keyed on employees' names.
; Furthermore, each employee's record is itself a set (structured differently
; from division to division) that contains information keyed under identifiers
; such as address and salary. In particular:
;
; a. Implement for headquarters a get-record procedure that retrieves a
; specified employee's record from a specified personnel file. The procedure
; should be applicable to any division's file. Explain how the individual
; divisions' files should be structured. In partucular, what type information
; must be supplied?
;
; b. Implement for headquarters a get-salary procedure that returns the salary
; information from a given employee's record from any division's personnel
; file. How should the record be structured in order to make this operation
; work?
;
; c. Implement for headquarters a find-employee-record procedure. This should
; search al the divisions' files for the record of a given employee and return
; the record. Assume that this procedure takes as arguments an employee's name
; and a list of all the divisions' files.
;
; d. WHen Insatiable takes over a new company, what changes must be made in
; order to incorporate the new personnel information into the central system?

; Alright then. Let's interpretate "a set of records" a bit loosely and have
; it as an s-expr, as opposed to having a specific set module. We'll have two
; divisions - Atreides and Fremen. Here's how their sets look like:

(define atreides
  '(("Paul Atreides"
     ((salary 2000)
      (address "Arrakeen Palace")))
    ("Gurney Halleck"
     ((salary 1500)
      (address "Here and there")))
    ("Duke Leto"
     ((salary 2500)
      (address "The Caladan planet")))))

(define fremen
  '(("Stilgar" .
    ((income . 1000)
     (location . "Sietch Tabr")))
    ("Chani" .
     ((income . 800)
      (location . "Whenever Paul is")))))

; Note that each division file has a type tag. Also note that while Atreides
; have each record as an a-list, Fremen have their records as a list of pairs.
; That way we have two data structures, which we should call a-lists and
; p-lists (for pair lists). Let's write some code for handling them:

(define (a-list-get a-list key)
  (cond ((null? a-list) '())
        ((equal? (caar a-list) key) (cadar a-list))
        (else (a-list-get (cdr a-list) key))))

(define (p-list-get p-list key)
  (cond ((null? p-list) '())
        ((equal? (caar p-list) key) (cdar p-list))
        (else (p-list-get (cdr p-list) key))))

; We will be working with tagged data, so we a couple of functions for that:

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum - TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum - CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types - APPLY-GENERIC" (list op type-tags))))))

; Finally, we need to implement the infrastructure we need for data-directed
; programming, i.e. the get and put procedures. We shall use Racket hashes for
; that one.

(define table (make-hash))

(define (put op type item)
  (hash-set! table (list op type) item))

(define (get op type)
  (hash-ref table (list op type)))

; a. get-record is just a simple generic procedure, implemented in a
; data-directed way. The generic implementation is below, while the specific
; implementation for each file is in the install-*-package procedure. Note
; that we attach a tag to the name to be consistent with apply-generic.

(define (get-record name file)
  (apply-generic 'get-record (attach-tag 'string name) file))

; b. get-salary is fairly similar to get-record:

(define (get-salary record)
  (apply-generic 'get-salary record))

; c. The list of divisions taken by find-employee-record should be type
; tagged. That said, here's the implementation:

(define (find-employee-record name division-files)
  (if (null? division-files)
      '()
      (let ((record (get-record name (car division-files))))
        (if (null? record)
            (find-employee-record name (cdr division-files))
            record))))

; Here are the Atreides and Fremen packages:

(define (install-atreides-package)
  (define (tag record)
    (if (null? record)
        record
        (attach-tag 'atreides record)))
  (define (get-record name file) (a-list-get file name))
  (define (get-salary record) (a-list-get record 'salary))

  (put 'get-record '(string atreides-file)
       (lambda (name file) (tag (get-record name file))))
  (put 'get-salary '(atreides) get-salary))

(define (install-fremen-package)
  (define (tag record)
    (if (null? record)
        record
        (attach-tag 'fremen record)))
  (define (get-record name file) (p-list-get file name))
  (define (get-salary record) (p-list-get record 'income))

  (put 'get-record '(string fremen-file)
       (lambda (name file) (tag (get-record name file))))
  (put 'get-salary '(fremen) get-salary))

; And here's how we install them:

(install-atreides-package)
(install-fremen-package)

; d. Whenever a new company is bought, there is only one change needed. The
; company needs to provide a procedure analogous to install-atreides-package
; that is able to work with their file.
;
; A question that is left unanswered is how to get the file for each division
; and tag it accodingly. One idea would be that each division has a name and
; we implement a procedure get-division-file that takes as an argument the
; division name and returns the file. This can be implemented in a number of
; ways, but all of those I can think of involve knowing something about state.
