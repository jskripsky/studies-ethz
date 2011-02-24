% bubble sort
%------------
bubble_sort(List,Sorted) :-
    bubble_sort(List,[],Sorted).

bubble_sort([],Acc,Acc).
bubble_sort([X|Xs],Acc,Sorted) :-
    bubble(X,Xs,Ys,Max),
    bubble_sort(Ys,[Max|Acc],Sorted).

bubble(X,[],[],X).
bubble(X,[Y|Ys],[Y|Zs],Max) :-
    X > Y,
    bubble(X,Ys,Zs,Max).
bubble(X,[Y|Ys],[X|Zs],Max) :-
    X =< Y,
    bubble(Y,Ys,Zs,Max).
