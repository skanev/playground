# Week 1 (2013-01-01 - 2013-01-06)

## The `let*` pattern

I just love using `let*` like in the solution of exercise 1.35:

    (let* ((node-data (contents-of tree))
           (lson-result (traverse (lson tree) counter))
           (counter-after-lson (car lson-result))
           (result-lson (cdr lson-result))
           (rson-result (traverse (rson tree) counter-after-lson))
           (result-counter (car rson-result))
           (result-rson (cdr rson-result))
           (result-tree (interior-node node-data result-lson result-rson)))
      (cons result-counter result-tree))))

It builds long computation, giving names to the intermediate values involved. In this particular computation, the algorithm might not be as clear as it would if I use a procedure to determine the count in the last node of lson, but it is still a great way to write it. I end up doing that quite often in JavaScript, where I would have code like this:

    var oldBag    = $('div[data-bag]'),
        newBag    = $(response),
        sizeError = $('#size-required'),
        title     = newBag.find('.title'),
        contents  = newBag.find('.contents');

I find this very readable, because all the right-hand sides of the assignment are short and use specific names defined earlier. It makes me focus on the code (instead of quickly scanning it), but I still find it very useful.

## Multiple levels of interface

I love this idea of multiple levels of interface that can be seen in exercise 2.03. There are four levels of abstraction.

1. The constructors and the observers that are a very thin wrap over the representation.
2. `minuend` and `subtrahend` that work with the previous layer to provide an uniform view of the two possible representations in the grammar.
3. The four operations for numbers that use minuend and subtrahend. Note that they also can be viewed as an interface implementation.
4. Operations built ontop of those four procedures like `plus`.

In an object-oriented language, all those layers are easily lost since they end up being in the same class.
