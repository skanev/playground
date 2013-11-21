# Week 07 (2013-11-13 - 2013-11-20)

## Various remarks

* It should have been obvious, but I never considered that a reverse-sorted
  array also produces the worst-case performance in quicksort.
* Quicksort is quadratic whenever one of the partitions has a constant size.
* There is a [killer adversary][killer-adversary] comparison algorithm for
  quicksort - it turns it quadratic in almost any implementation.

[killer-adversary]: http://www.cs.dartmouth.edu/~doug/mdmspe.pdf
