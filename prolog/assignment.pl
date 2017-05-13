%%% Setup.
female(ziva).
female(soca).
female(frances).

male(barak).
male(fishel).

parent(barak,ziva).
parent(barak,soca).
parent(fishel,barak).
parent(frances,barak).

grandparent(frances,jim).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

%%% Q1
interleave([], [], []).
interleave([X|Xs], Ys, [X|Zs]) :- interleave(Ys, Xs, Zs).

%%% Q2
% Naive version.
% Always matches grandchildren with themselves, because obviously they
% have a common grandparent with themselves.
% Matches siblings with a grandparent, because there is no clause to
% prevent that.
cousin(X, Y) :- grandparent(Z, X), grandparent(Z, Y).

% Fixed version:
sibling(X, Y) :- parent(Z,X), parent(Z,Y).
cousin2(X, Y) :- grandparent(Z, X), grandparent(Z, Y), X \= Y, not(sibling(X,Y)).