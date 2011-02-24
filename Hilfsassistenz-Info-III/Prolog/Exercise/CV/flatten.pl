flatten(Xs,Ys) :- flatten(Xs,Ys,[]).
flatten([X|Xs],Ys,Zs) :-
    flatten(X,Ys,Ys1),
    flatten(Xs,Ys1,Zs).
flatten(X,[X|Xs],Xs) :-
    atomic(X),
    X \== [].
flatten([],Xs,Xs).