# Week 17 (2012-12-11 - 2012-12-18)

## Notes

This week was all abount implementing the logic programming environments. I don't have much insight to share. I have, however, written a lot of code. I got a lot of pleasant flashbacks to the time in FMI when we were learning Prolog. I wanted to write a Prolog implementation in the summer after the second year, but I never got around to do it. I still want to do it some time. There is a book that might be useful - [An Introduction to Logic Programming through Prolog][logic-programming].

[logic-programming]: http://spivey.oriel.ox.ac.uk/corner/Logic_Programming

## The predicate? convention and the query language

Every time I write a rule or assertion in the query language, I have the urge to postfix it with a question mark (?) to indicate that it is a predicate. This makes no sense, since all the rules and assertions are in fact predicates. This is an important point about notation - you introduce it to distinguish between different things. Since there are no non-predicates in the query data base, the question mark does not indicate anything and therefore it is a useless notation.
