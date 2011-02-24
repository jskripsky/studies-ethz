/*********************** generate_table.pl ****************************/
/****** module for generating the result in a table *******************/
/**********************************************************************/
/*** Author: Claudia Ignat ********* Date: 6.12.2001 ******************/
/**********************************************************************/

:-module(generate_table,[generate_table/1]).

generate_table(Marriages):-
      format("~`*t RECOMMENDED PARTNERS ~`*t~61|~n", []),
      format("*~t*~61|~n", []),
      format("*   ~a~t~20|~a~t~20+~a~t~20+~t*~61|~n",
                   ['Crt.','Man','Woman']),
      format("*   ~a~t~20|~t*~61|~n", ['No.']),
      format("~`*t~61|~n", []),
      display_pairs(1,Marriages),
      format("~`*t~61|~n", []).

display_pairs(_,[]).
display_pairs(N,[marriage(M,W)|Marriages]):-
      format("*   ~d~t~20|~a~t~20+~a~t~20+~t*~61|~n", [N,M,W]),
      N1 is N+1,
      display_pairs(N1,Marriages).