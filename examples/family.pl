% Family relationships example
% Demonstrates facts, rules, and basic queries

% Facts: parent(Parent, Child)
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Rules
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

sibling(X, Y) :- parent(P, X), parent(P, Y).

% Query: Is tom an ancestor of jim?
?- ancestor(tom, jim).
