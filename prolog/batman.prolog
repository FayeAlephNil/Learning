batman(bruce).
sidekick(robin).

manDressedLikeABat(X) :- batman(X).
manDressedLikeABird(robin).

clown(joker).
clown(henchmen).

evil(X) :- clown(X).
good(X) :- batman(X); sidekick(X).
neutral(X) :- not(good(X)), not(evil(X)).

enemies(X, Y) :-
  good(X),
  evil(Y).

allies(X, Y) :-
  (good(X), good(Y));
  (evil(X), evil(Y)).
allies(bruce, twoFace).
allies(joker, twoFace).

split(X) :-
  (allies(X, Y); allies(Y, X)),
  (allies(X, Z); allies(Z, X)),
  enemies(Y, Z).
