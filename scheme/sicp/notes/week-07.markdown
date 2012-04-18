# Week 07 (2012-04-10 - 2012-04-17)

This week I barely did anything. I just managed to complete 3 exercises, once of which in a very poor fashion. I was planning to have tons of free time around Easter, but that's life for you - making plans is the surest way to make your deity laugh.

## Notes

* `queens` in 2.42 is written independent of the presentation. `empty-board` contributes to that. Neat. However, this forces a crappy implementation.
* I can't help but think that 2.42 can be written way better if the required functions are implemented via another layer of data abstraction.

## Top-down versus bottom-up test-driving

It is hard to test-drive 2.42 (eight queens puzzle) in an outside-in fashion. There are three reasons for that:

* There is a scaffold of the procedure that gets in the way of writing it incrementally.
* It will force us to decide on representation, which we don't want to do.
* We don't know the solutions.

In this sense, it makes to do a bottom-up test-drive. The upsides are obvious. There are downsides, though - (1) we can throw away a function we just test-drove or (2) we can invalidate all our tests by changing representation. Both of them happened to me.

The take away is: when doing up bottom-up test-driving, do more up-front planning.

## Meetup summary

* Veselin suggests to check out [(fluxus)][fluxus].
* Plamen is playing a lot with Gimp and visualizing the painters in it.
* I suggest that this should all happen with shelling out to ImageMagick. Or the Racket Drawing Kit.
* My eight queens suck. I should redo them nicely.

[fluxus]: http://www.pawfal.org/fluxus/
