batman(bruce).
sidekick(robin).

manDressedLikeABat(X) :- batman(X).
manDressedLikeABird(robin).

clown(joker).
clown(henchmen).

evil(X) :- clown(X).
good(X) :- batman(X); sidekick(X).

enemies(X, Y) :-
  good(X),
  evil(Y).
party.
