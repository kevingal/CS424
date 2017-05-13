% 2015A, q3
noah([], [], []).
noah([X|XS], [Y|YS], [X,Y|ZS]) :- noah(XS, YS, ZS).

% 2015J, q4
doublemember(X, [X|XS]) :- member(X,XS).
doublemember(X, [Y|YS]) :- doublemember(X,YS).

member(X,[X|XS]).
member(X,[Y|XS]) :- member(X,XS).

% 2014A, q3
sibling([[Z,X]|PS], X, Y) :- member([Z,Y], PS).
sibling([[Z,Y]|PS], X, Y) :- member([Z,X], PS).
sibling([[_,_]|PS], X, Y) :- sibling(PS, X, Y).

% 2014J, q3
noah2([], []).
noah2([X,Y|ZS], [[X,Y]|PS]) :- noah2(ZS, PS).

%%
% 99 problems but logic ain't one.
% http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
%%
% 1
my_last(X,[X]).
my_last(X,[Y|YS]) :- my_last(X,YS).

% 2
last_but_one(X,[X,_]).
last_but_one(X,[Y|YS]) :- last_but_one(X,YS).

% (neither of these are working).

% 3
element_at(X,[X|_],1).
element_at(X,[Y|YS],N) :- element_at(X,YS,N-1).

% 4
my_length(0,[]).
my_length(N,[_|XS]) :- N1 is N - 1, my_length(N1,XS).


%%
% Misc
%%
app([],YS,YS).
app([X|XS],YS,[X|ZS]) :- app(XS,YS,ZS).