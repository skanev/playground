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

firsth(P, [P, _, _, _, _]).
leftof(A, B, List) :- nextto(A, B, List).
center(P, [_, _, P, _, _]).

neighb(A, B, List) :- nextto(A, B, List).
neighb(A, B, List) :- nextto(B, A, List).

solution(People) :-
  People = [_, _, _, _, _],

  member([brit,      red,    _,      _,      _          ], People),
  member([swede,     _,      dogs,   _,      _          ], People),
  member([dane,      _,      _,      tea,    _          ], People),
  leftof([_,         green,  _,      _,      _          ],
         [_,         white,  _,      _,      _          ], People),
  member([_,         green,  _,      coffee, _          ], People),
  member([_,         _,      birds,  _,      pall_mall  ], People),
  member([_,         yellow, _,      _,      dunhill    ], People),
  center([_,         _,      _,      milk,   _          ], People),
  firsth([norwegian, _,      _,      _,      _          ], People),
  neighb([_,         _,      _,      _,      blends     ],
         [_,         _,      cats,   _,      _          ], People),
  neighb([_,         _,      horses, _,      _          ],
         [_,         _,      _,      _,      dunhill    ], People),
  member([_,         _,      _,      beer,   blue_master], People),
  member([german,    _,      _,      _,      prince     ], People),
  neighb([norwegian, _,      _,      _,      _          ],
         [_,         blue,   _,      _,      _          ], People),
  neighb([_,         _,      _,      _,      blends     ],
         [_,         _,      _,      water,  _          ], People),

  maplist(person, People),
  flatten(People, Items),
  is_set(Items).

has_fish(Nationality) :- solution(Solution), member([Nationality, _, fish, _, _], Solution).
