(require rackunit rackunit/text-ui)
(load "../45.scm")

(define (all-values exp)
  (ambeval exp
           solution-environment
           (lambda (value fail) (cons value (fail)))
           (lambda () '())))

(define sicp-4.45-tests
  (test-suite
    "Tests for SICP exercise 4.45"

    (check-equal? (all-values
                    '(parse
                       '(the professor lectures to the student in the class with the cat)))
                  '((sentence
                      (simple-noun-phrase (article the) (noun professor))
                      (verb-phrase
                        (verb-phrase
                          (verb-phrase
                            (verb lectures)
                            (prep-phrase
                              (prep to)
                              (simple-noun-phrase (article the) (noun student))))
                          (prep-phrase
                            (prep in)
                            (simple-noun-phrase (article the) (noun class))))
                        (prep-phrase
                          (prep with)
                          (simple-noun-phrase (article the) (noun cat)))))

                    (sentence
                      (simple-noun-phrase (article the) (noun professor))
                      (verb-phrase
                        (verb-phrase
                          (verb lectures)
                          (prep-phrase
                            (prep to)
                            (simple-noun-phrase (article the) (noun student))))
                        (prep-phrase
                          (prep in)
                          (noun-phrase
                            (simple-noun-phrase (article the) (noun class))
                            (prep-phrase
                              (prep with)
                              (simple-noun-phrase (article the) (noun cat)))))))

                    (sentence
                      (simple-noun-phrase (article the) (noun professor))
                      (verb-phrase
                        (verb-phrase
                          (verb lectures)
                          (prep-phrase
                            (prep to)
                            (noun-phrase
                              (simple-noun-phrase (article the) (noun student))
                              (prep-phrase
                                (prep in)
                                (simple-noun-phrase (article the) (noun class))))))
                        (prep-phrase
                          (prep with)
                          (simple-noun-phrase (article the) (noun cat)))))

                    (sentence
                      (simple-noun-phrase (article the) (noun professor))
                      (verb-phrase
                        (verb lectures)
                        (prep-phrase
                          (prep to)
                          (noun-phrase
                            (noun-phrase
                              (simple-noun-phrase (article the) (noun student))
                              (prep-phrase
                                (prep in)
                                (simple-noun-phrase (article the) (noun class))))
                            (prep-phrase
                              (prep with)
                              (simple-noun-phrase (article the) (noun cat)))))))

                    (sentence
                      (simple-noun-phrase (article the) (noun professor))
                      (verb-phrase
                        (verb lectures)
                        (prep-phrase
                          (prep to)
                          (noun-phrase
                            (simple-noun-phrase (article the) (noun student))
                            (prep-phrase
                              (prep in)
                              (noun-phrase
                                (simple-noun-phrase (article the) (noun class))
                                (prep-phrase
                                  (prep with)
                                  (simple-noun-phrase (article the) (noun cat)))))))))))
))

(run-tests sicp-4.45-tests)
