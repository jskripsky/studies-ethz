flatten_dr([X|Xs],Ys) :-
    flatten_dr(X,Ys1),
    flatten_dr(Xs,Ys2),
    append(Ys1,Ys2,Ys).
flatten_dr(X,[X]) :-
    atomic(X),
    X \== [].
flatten_dr([],[]).

flatten_is(Xs,Ys) :- flatten_is(Xs,[],Ys).

flatten_is([X|Xs],S,Ys) :-
    list(X),
    flatten_is(X,[Xs|S],Ys).
    flatten_is([X|Xs],S,[X|Ys]) :-
    atomic(X),
    X \== [],
    flatten_is(Xs,S,Ys).
flatten_is([],[X|S],Ys) :-
    flatten_is(X,S,Ys).
flatten_is([],[],[]).

list([_|_]).

flatten_dl(Xs,Ys) :- flatten_dl(Xs,Ys,[]).

flatten_dl([X|Xs],Ys,Zs) :-
    flatten_dl(X,Ys,Ys1),
    flatten_dl(Xs,Ys1,Zs).
flatten_dl(X,[X|Xs],Xs) :-
    atomic(X),
    X \== [].
flatten_dl([],Xs,Xs).

flatten_dcg(Xs,Ys) :- flatten_dcg(Xs,Ys,[]).

flatten_dcg([X|Xs]) --> flatten_dcg(X), flatten_dcg(Xs).
    flatten_dcg(X) --> { atomic(X), X \== [] }, [X].
flatten_dcg([]) --> [].


 
