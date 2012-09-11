# Week 13 (2012-08-28 - 2012-09-03)

## Notes

* Racket does not support `set-car!` and `set-cdr!`. There is a [blog post][getting-rid] explaining why. While certainly unnice, it can be performed either with `mcons` and friends or by using "legacy Scheme" by doing `(require r5rs/init)`.
* Dynamic scoping is very cool, because it allows doing `with-output-to-string`. This allows overriding the default print port, thus capturing the output from `display`. That way the printing side-effect can be tested. I just love that.
