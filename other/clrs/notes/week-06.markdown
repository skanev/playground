# Week 06 (2013-11-06 - 2013-11-13)

Last week was OpenFest, so I skipped one week. This week it is heap-sort time!

## Questions

* I'm not really sure why `BUILD-MAX-HEAP` is linear. I understand the math in
  section 6.3, but it is the weirdest approach ever. I wonder how people come
  up with that stuff.

## Various remarks

* I really need to learn gdb. While I don't find debugging all that necessary
  in Ruby, all the minute details appear important in this context. Unit tests
  are not enough to give you a good tangent.
* An example of the previous point is exercise 6.2-5. It took me a while to
  figure out that I was passing `expected` instead of `actual` to `MAX-HEAPIFY`
  in the unit test.
* The exercises in this book are extremely well-designed. They complement the
  material very well and doing them expands your knowledge. For example, the
  heap sort bounds are explored in exercises.
* The lower bound of heap-sort was tricky. It was found some 30 years after
  the algorithms was discovered. Exercise 6.4.5 dwells on it.
