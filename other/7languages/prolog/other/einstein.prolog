/*
  Einstein's riddle

  In a street there are five houses, painted five different colors.
  In each house lives a person of different nationality.
  These five homeowners each drink a different kind of beverage, smoke
  different brand of cigar and keep a different pet.

    1. The Brit lives in a red house.
    2. The Swede keeps dogs as pets.
    3. The Dane drinks tea.
    4. The green house is next to, and on the left of the white house.
    5. The owner of the green house drinks coffee.
    6. The person who smokes Pall Mall rears birds.
    7. The owner of the yellow house smokes Dunhill.
    8. The man living in the centre house drinks milk.
    9. The Norwegian lives in the first house.
    10. The man who smokes Blends lives next to the one who keeps cats.
    11. The man who keeps horses lives next to the man who smokes Dunhill.
    12. The man who smokes Blue Master drinks beer.
    13. The German smokes Prince.
    14. The Norwegian lives next to the blue house.
    15. The man who smokes Blends has a neighbour who drinks water.

  Who owns the fish?
*/

nationality(Nationality) :- member(Nationality, [brit, swede, dane, norwegian, german]).
color(Color) :- member(Color, [green, yellow, white, red, blue]).
pet(Pet) :- member(Pet, [fish, dogs, birds, horses, cats]).
beverage(Beverage) :- member(Beverage, [tea, coffee, milk, beer, water]).
cigars(Brand) :- member(Brand, [pall_mall, dunhill, blends, prince, blue_master]).

person([Nation, Color, Pet, Beverage, Cigars]) :- nationality(Nation), color(Color), pet(Pet), beverage(Beverage), cigars(Cigars).

has(Thing, Person) :- nationality(Thing), Person = [Thing, _, _, _, _].
has(Thing, Person) :- color(Thing), Person = [_, Thing, _, _, _].
has(Thing, Person) :- pet(Thing), Person = [_, _, Thing, _, _].
has(Thing, Person) :- beverage(Thing), Person = [_, _, _, Thing, _].
has(Thing, Person) :- cigars(Thing), Person = [_, _, _, _, Thing].

same(A, B, People) :- has(A, Person), has(B, Person), member(Person, People).

first_house(A, People) :- has(A, Person), People = [Person, _, _, _, _].
center_house(A, People) :- has(A, Person), People = [_, _, Person, _, _].
left_of(A, B, People) :- has(A, PersonA), has(B, PersonB), nextto(PersonA, PersonB, People).
neighbours(A, B, People) :- left_of(A, B, People); left_of(B, A, People).

solution(People) :-
  People = [_, _, _, _, _],

  same(brit, red, People),
  same(swede, dogs, People),
  same(dane, tea, People),
  left_of(green, white, People),
  same(green, coffee, People),
  same(birds, pall_mall, People),
  same(yellow, dunhill, People),
  center_house(milk, People),
  first_house(norwegian, People),
  neighbours(blends, cats, People),
  neighbours(horses, dunhill, People),
  same(beer, blue_master, People),
  same(german, prince, People),
  neighbours(norwegian, blue, People),
  neighbours(blends, water, People),

  maplist(person, People),
  flatten(People, Items),
  is_set(Items).

has_fish(Nationality) :- solution(Solution), member([Nationality, _, fish, _, _], Solution).
