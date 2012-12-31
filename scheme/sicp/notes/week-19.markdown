# Week 19 (2012-12-25 - 2012-12-31)

I finally finished the book. By the end it was a huge crunch. I finished chapter 5 in two weeks and I was working really hard to do so.

## Internal defines revisited

The discussion on why internal defines need to be scanned out finally started making sense when we wen't into lexical addressing. Basically, if internal defines are not scanned out (and converted to a let statement), lexical addressing just won't work.

## Writing C again

I had not written C since high school and revisiting it was an interesting experience. I could automate a lot of things with rake and watchr, but debugging it was still very hard. I did not use a debugger and just scattered `printf()` calls in the code. In retrospective, I should have taken some time to figure out gdb and used it instead. A huge pain point was debugging the garbage collector. I could not figure out why some memory got collected and I modified the C code to dump the memory and used a Ruby program to analyze it. The lack of introspection in C is really hurtful. Furthermore, C code tends to be very verbose and the lack of nice types (namely hashes and lists) makes it tricky to write code to figure out why something happens. Jakob said that he likes C because "it punishes you really hard if you don't think a lot before you write" and I've started to see what he means by that.

## Prototyping, C and refactorability

I've developed a new appreciation of the idea of writing a prototype in a higher-level language. Doing exploratory development with C is just too hard. You end up defining a lot of stuff and then changing it because you discover a better design. I'm starting to think that "refactorability" is an important feature of the language and the more less refactorable a language is, the more up-front design you have to do. It's easy to refactor Ruby, since there is not that much to write and you can get through the intermediate states with consice (if cryptic) expressions. In C, you have to write a lot.
