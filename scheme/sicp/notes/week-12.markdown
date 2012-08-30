# Week 12 (2012-08-21 - 2012-08-28)

As the git history clearly indicates, we dropped the ball at some point and I stopped solving exercises. This I picked it up. Nikolay started a new trend since Chapter 3 - now when we meet we don't discuss the exercise, but we read the text instead. That's actually a lot of fun. On the downside, we the progress is slower.

I just finished a huge chunk of exercise and I am going to make some points.

## Meetup summary

We read 3.3.5 Propagation of Constraints. The model there is quite interesting, although there was nothing groundbreaking in it.

## Data-directed programming

The data-directed programming introduced in 2.4.3 is a very interesting idea. It is quite similar to multi-dispatch. It involves building a lot of infrastructure and slowing down the execution, but in the end, it has some interesting advantages. First, it solves the structures vs. classes problem to some extend. It allows adding both a new operation and a new type without resulting to Shotgun Surgery. It gets a bit more complicated when there are two separate packages, where the first one introduces a new type while the second - a new operation. The new types does not support the new operation if one of the package does not know about the other. This is a curious issue, but I don't think there is a good solution.

## Systems with Generic Operations

Section 2.5 was a lot of fun. I had to implement generic arithmetic operations over a bunch of numbers, including complex numbers with multiple representations and polynomials with both multiple representations and mixed-type coefficients. An interesting thing to note is when the types and operations are not organized in classes, they are way nicer to implement. For example, there is no need to have Python's `__radd__` and `__rmul`__ to support `3 * Vector(1, 2)`. It still feels that there can be more abstraction on operations (symmetrical opreations for example) but I don't dare venture there without knowing how to write macros. Or rather, I dare, but I'm too lazy to build complex procedures that take too many lambdas.

## Coercion

Coercion was implemented in a suboptimal way, since it involved raising types and getting arguments to the same type. This makes sense if we're extremely duck-typed and we don't know what types the operation is defined for. Our table, however, allows us to inspect what types an operation is defined for and we should be able to determine which procedure to invoke and what type conversion to do. This is an interesting problem I don't know much about, but I'm guessing that they avoided it because (1) it can be quite fat and (2) we don't know enough about state in order to inspect the table. I'm curious whether this topic will be revisited. I should also consider creating a showcase for this system once I learn how to do macros (since they are never introduced in this book).
