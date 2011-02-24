%---------------------------------
%---------------------------------
% Some examples related to sorting
%---------------------------------
%---------------------------------




% predicate to check if list of integers is sorted
%-------------------------------------------------
is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|T]) :- 
    X =< Y,
    is_sorted([Y|T]).



% predicate to relate a list to a permutation of that list
%---------------------------------------------------------
perm([],[]).
perm([X|Xs],Ys) :-
    perm(Xs,Zs),
    insert(X,Zs,Ys).

insert(X,Xs,[X|Xs]).
insert(X,[Y|Ys],[Y|Zs]) :-
    insert(X,Ys,Zs).



% a very naive and inefficient sort!
%-----------------------------------
naive_sort(List,Sorted) :-
    perm(List,Sorted),
    is_sorted(Sorted).




% insert element in an ordered list
%----------------------------------
ord_insert(X,[],[X]).
ord_insert(X,[Y|Ys],[X,Y|Ys]) :-
    X =< Y.
ord_insert(X,[Y|Ys],[Y|Zs]) :-
    X > Y,
    ord_insert(X,Ys,Zs).




% insertion sort
%---------------
insert_sort(List,Sorted) :-
    insert_sort_1(List,[],Sorted).

insert_sort_1([],Acc,Acc).
insert_sort_1([X|Xs],Acc,Sorted) :-
    ord_insert(X,Acc,Acc1),
    insert_sort_1(Xs,Acc1,Sorted).




% What kind of sort is this?
%---------------------------
a_sort(List,Sorted) :-
    a_sort_1(List,[],Sorted).

a_sort_1([],Acc,Acc).
a_sort_1([X|Xs],Acc,Sorted) :-
    aux(X,Xs,Ys,Max),
    a_sort_1(Ys,[Max|Acc],Sorted).

aux(X,[],[],X).
aux(X,[Y|Ys],[Y|Zs],Max) :-
    X > Y,
    aux(X,Ys,Zs,Max).
aux(X,[Y|Ys],[X|Zs],Max) :-
    X =< Y,
    aux(Y,Ys,Zs,Max).



% Write a quicksort in Prolog
%----------------------------

quick_sort(_,_) :-
   write('Not yet implemented'), nl.



% Generalised insert sort
%------------------------

lesser(X,Y,X) :- 
   X =< Y.
lesser(_,Y,Y).

greater(X,Y,X) :- 
   X >= Y.
greater(_,Y,Y).

gen_insert_sort(List,Sorter,Sorted) :-
  gen_insert_sort_1(List,Sorter,[],Sorted).

gen_insert_sort_1([],_,Acc,Acc).
gen_insert_sort_1([X|Xs],Sorter,Acc,Sorted) :-
  gen_ord_insert(X,Sorter,Acc,Acc1),
  gen_insert_sort_1(Xs,Sorter,Acc1,Sorted).

gen_ord_insert(X,_,[],[X]).
gen_ord_insert(X,Sorter,[Y|Ys],[X,Y|Ys]) :-
  Goal =.. [Sorter,X,Y,X],
  Goal, !.
gen_ord_insert(X,Sorter,[Y|Ys],[Y|Zs]) :-
  gen_ord_insert(X,Sorter,Ys,Zs).


