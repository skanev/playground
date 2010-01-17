(module bintree-to-list-test scheme
  (require (planet schematics/schemeunit:3)
           (planet schematics/schemeunit:3/text-ui)
           "bintree-to-list.ss")

  (define bintree-to-list-tests
    (test-suite
      "bintree-to-list function"

      (check-equal? (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
                                    '(interior-node  a (leaf-node 3) (leaf-node 4)))
  ))

  (run-tests bintree-to-list-tests)
)
