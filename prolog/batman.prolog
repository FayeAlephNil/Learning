batman(bruce).
sidekick(robin).

manDressedLikeABat(X) :- batman(X).
manDressedLikeABird(robin).

clown(joker).
clown(henchmen).

evil(X) :- clown(X), not(good(X)).
good(X) :- (batman(X); sidekick(X)), not(evil(X)).
neutral(X) :- not(good(X); evil(X)).

enemies(X, Y) :-
  good(X),
  evil(Y).

allies(X, Y) :-
  ((good(X), good(Y));
  (evil(X), evil(Y))),
  not(X = Y).

allies(bruce, twoFace).
allies(joker, twoFace).

neutral(X, Y) :-
  not(allies(X, Y); allies(Y, X)),
  not(enemies(X, Y); enemies(Y, X)).

split(X) :-
  (allies(X, Y); allies(Y, X)),
  (allies(X, Z); allies(Z, X)),
  enemies(Y, Z).
