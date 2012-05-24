# Week 08 (2012-04-17 - 2012-04-24)

This week was BaconConf. I was very productive and did a lot of progress.

## Notes

* My estimate on 2.43 is a bit off. I should check or ask around.
* I'm testing 2.44 in a funky way. I'm replacing below and besides with procedures that construct lists and am asserting on the resulting list.
* Racket has a plt-r5rs executable. Maybe I can use it instead?
** Plamen said it did not really live up to its name.
* Matrix transformations are fun!

## The Rakefile

Writing the Rakefile was immense fun. It also optimized a lot of things in my process. I realize, that spending time in order to build an environment where you can be effective is very, very important and totally worth investing in. For two reasons.

First, you get the productivity boost. You automate repetative tasks and you reduce the time to accomplish your goal.

Second, and more important, you create an environment you can keep growing. If you need a new feature, you won't have to start from scratch - you have a place to put it and you probably have some code to build ontop of. It's pretty similar to the dotfiles and the Vim configuration. There is a general, underlying concept here, that I need to pinpoint.

## Procedural representation is a hack

While it is a very nice showcase of the power of programming languages, procedural representation feels like a hack to me. Consider implementing pairs as lambdas. It has a bunch of problems:

* No types. There is no way to define `pair?`.
* Either it is limited to one selector or it has some awkwardness.
* There is no way to introspect it.
* It is hard to manipulate the underlying structure. You can just invoke the selector.

There might be a deeper application I have not thought of. And it is occasionally very pragmatic (e.g. the picture language). But it feels hacky for me.

## Meetup summary

We did not hold a meetup, because half of us were ill.
