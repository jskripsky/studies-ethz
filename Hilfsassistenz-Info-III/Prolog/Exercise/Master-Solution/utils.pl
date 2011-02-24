/******************** utils.pl ***************************************/
/*********************************************************************/
/******** Author: Claudia Ignat ******** Date:6.12.2001 **************/
/*********************************************************************/

:-module(utils, [no_common_elements/3, convert/2, check/3]).
:-use_module(library(lists)).

/* finds the number of common elements */
/* of two lists */
no_common_elements([], _, 0):-!.
no_common_elements(_, [], 0):-!.
no_common_elements([H|Tail], List2, N):-
         select(H, List2, Rest),!,           % select was used instead of
                                             % member for optimisation
         no_common_elements(Tail, Rest, N1),
         N is N1+1.
no_common_elements([_|Tail], List2, N):-
         no_common_elements(Tail, List2, N).


/* converts a list of attractiveness levels into */
/* a list of preferences */
convert([], []).
convert([a(X,_)|Attracts], [X|Prefs]):-
         convert(Attracts, Prefs).

/* check is true if X is in the list given as 2nd argument */
/* the 3rd arg. being the rest of the list after the occurence  of X */
/* Example:  for check(3, [2, 4, 3, 1, 7], R), the answer is R = [1,7] */
check(X, [X|T], T):-!.
check(X, [_|T], R):-check(X, T, R).

