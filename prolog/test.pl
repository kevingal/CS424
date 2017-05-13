doublemember(X, [X|Xs]) :- member(X, Xs).
doublemember(X, [Y|Ys]) :- doublemember(X, Ys).

noah([], []).
noah([X,Y|As], [[X,Y]|Bs]) :- noah(As, Bs).

sibling([[A,X]|Cs], X, Y) :- member([A,Y], Cs).
sibling([[A,Y]|Cs], X, Y) :- member([A,X], Cs).
sibling([[A,B]|Cs], X, Y) :- sibling(Cs, X, Y).

noah2([],[],[]).
noah2([A|As], [B|Bs], [A,B|Xs]) :- noah2(As, Bs, Xs).

