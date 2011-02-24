:- use_module(library(lists)).

% quick sort
%-----------
quick_sort([], []).
quick_sort([Median|Xs], Sorted) :-
   partition(Xs, Median, Smaller, Bigger),
   quick_sort(Smaller, SmallSorted),
   quick_sort(Bigger, BigSorted),
   append(SmallSorted, [Median|BigSorted], Sorted).



partition([], _, [], []).

partition([X|Xs], Median, [X|Smaller], Bigger) :-
   X =< Median,
   partition(Xs, Median, Smaller, Bigger).

partition([X|Xs], Median, Smaller, [X|Bigger]) :-
   X > Median,
   partition(Xs, Median, Smaller, Bigger).

