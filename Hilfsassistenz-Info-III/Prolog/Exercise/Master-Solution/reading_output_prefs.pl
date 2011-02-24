/*********************** reading.pl ********************************/
/**** module for processing the input ******************************/
/*******************************************************************/
/******** Author: Claudia Ignat ****** Date: 6.12.2001 *************/
/*******************************************************************/

:- module(reading, [input_names/1, input_preferences/2, 
                    input_lives_in/1, input_interests/1,
                    get_preferences/2]). 
:- dynamic interests/2, lives_in/2, preferences/2.

:- use_module(utils,[no_common_elements/3, convert/2]).


/* taken from ELIZA program and slightly modified*/
/* from the input returns a list of words */
read_word_list(Ws) :-
        get0(C),
        read_word_list(C,Ws).

read_word_list(C,[]) :-
        end_of_words_char(C).
read_word_list(C,Ws) :-
        fill_char(C),!,
        get0(C1),
        read_word_list(C1,Ws).
read_word_list(C,[W|Ws]) :-
        word_char(C,C1),
        read_word(C1,W,C2),
        read_word_list(C2,Ws).

read_word(C,W,C1) :-
        word_chars(C,Cs,C1),
        name(W,Cs).

word_chars(C,[C1|Cs],C0) :-
        word_char(C,C1), !,
        get0(C2),
        word_chars(C2,Cs,C0).
word_chars(C,[],C).

%  characters occurring in words of user sentences
%  can be only letters
word_char(C,C) :- 97 =< C, C =< 122.
word_char(C,C1) :- 65 =< C, C =< 90, C1 is C + 32.

%  symbols considered as word separators
fill_char(32).        % space

%  sentences ended by return
end_of_words_char(10).


/*************************************************************/
/* input a list of names                                     */
/* L will be instantiated to the list of names               */
/*************************************************************/
input_names(L):- 
        read_word_list(L),!.
input_names(L):-
	nl, write('  The list of names should consist of names (only letters)'),
        nl, write('  separated by spaces and terminated by newline'), 
	skip_line, nl, nl,
        write('Input again the list of names: '),
	input_names(L).


/*************************************************************/
/* for each person in a list input the city he/she lives in  */
/*************************************************************/
/*           input_lives_in(+L)                              */
/*   L - the list of persons                                 */
/*************************************************************/
input_lives_in([]).
input_lives_in([X|T]):-
         write('   City of '), write(X), write(': '),
         read_word_list([City]),!,
         assertz(lives_in(X, City)),
         input_lives_in(T).
input_lives_in(L):-
	 nl, write('   The name of the city consists of one word'),
         nl, write('   (only letters) followed by newline'),
         nl, write('   Try again!'), skip_line, nl,nl,
         input_lives_in(L).
	


/*************************************************************/
/* for each person in a list input his/her interests         */
/*************************************************************/
/*           input_interests(+L)                             */
/*   L - the list of persons                                 */
/*************************************************************/
input_interests([]).
input_interests([X|T]):-
         write('   Interests of '), write(X), write(': '),
	 read_word_list(Interests),!,
	 process_interests([X|T],Interests).   
input_interests(L):-
         nl, write('   Every interest consists of a word'),
         nl, write('   composed only from letters '),
	 nl, write('   Try again!'),
	 skip_line, nl, nl,
         input_interests(L).

/* checks if the list of interests for the current person, */
/* i.e. the person represented by the head of the first list */
/* given as argument, consists of at most 5 interests */
/* if yes, assert the list of interests for that person and continue */
/* to input the interests for the other persons*/
/* if no, let the user to reenter the list of interests for the */
/* current person */
process_interests([X|T],Interests):-
	 length(Interests, L),
         L =< 5,!,
	 assertz(interests(X, Interests)),
         input_interests(T).
process_interests(L,_):-
         nl, write('   The number of interests must be at most 5'),
	 nl, write('   Try again!'),nl,nl,
         input_interests(L).


/***************************************************/
/* for a list of persons read in turn for each     */
/* person the degree of attractiveness that person */
/* assigns to each person from the second list     */
/***************************************************/
input_preferences([],_).
input_preferences([H|T],L):-
         write('How attractive finds '), write(H), nl,
         input_pref(H, L, []),
         input_preferences(T,L).

/*************************************************************/
/* for a given person read the degree of attractiveness      */
/* that person assigns to each person from the second list   */
/*************************************************************/
/*          input_pref(+X, +L, -Attracts)                    */
/*    X        - the given person                            */
/*    L        - the list of persons for which there must be */
/*               assigned the attractiveness levels          */
/*    Attracts - the list of attractiveness levels           */
/*************************************************************/
input_pref(X, [], Attracts):-
         convert(Attracts, Prefs),
         assertz(preferences(X,Prefs)),
         write(X), write(Prefs).
input_pref(X, [Y|T], Attracts):-
         write('        '), write(Y), write(' '), read(N),
         integer(N), N >= 1, N =< 10, !,
         insert_in_order(X,Y,N,Attracts,Attracts1),
         input_pref(X, T, Attracts1).
input_pref(X, [Y|T], Attracts):-
         nl, write('  The degree of preference should be an integer '),
	 nl, write('  in the range [1,10]'),
	 nl, write('  Try again!'), nl,
         nl, write('How attractive finds '),  write(X), nl,
         input_pref(X, [Y|T], Attracts).



/* We call list of atractiveness levels corresponding */
/* to a person X, the list with elements of the form a(Y,N), */
/* where Y is a person of the opposite sex as X and N is the */
/* degree of attractiveness of person Y for person X. */
/* The elements of the list are stored in a descending order */
/* of their levels of attractiveness */

/* inserts a new person associated with a level of */
/* attractiveness into the list of attractiveness */
/* levels corresponding to a person */
insert_in_order(_,Y,N,[],[a(Y,N)]).
insert_in_order(X,Y,N,[a(Z,M)|T],[a(Z,M)|R]):-        % if the degree of
         M>N,!,                                       % attractiveness of X
         insert_in_order(X,Y,N,T,R).                  % for Z is > degree of
                                                      % attractiveness of X
                                                      % for Y

insert_in_order(_,Y,N,[a(Z,M)|T],[a(Y,N),a(Z,M)|T]):- % if the degree of
         M<N,!.                                       % attractiveness of X
                                                      % for Z is > degree of
                                                      % attractiveness of X
                                                      % for Y

insert_in_order(X,Y,N,[a(Z,M)|T],Res):-          % if degrees of attract.
         interests(X, InterestsX),               % of X for Y and Z are equal
         interests(Y, InterestsY), 
	 interests(Z, InterestsZ),           
         no_common_elements(InterestsX, InterestsY, NoCommXY),
         no_common_elements(InterestsX, InterestsZ, NoCommXZ),
         (NoCommXZ > NoCommXY ->             % X&Z have more common
               insert_in_order(X,Y,N,T,R),   % interests than X&Y
               Res = [a(Z,M)|R]           
             ; NoCommXZ < NoCommXY,          % X&Z have less common
               Res = [a(Y,N),a(Z,M)|T]       % interests than X&Y
         ),!. 

insert_in_order(X,Y,N,[a(Z,M)|T],[a(Z,M)|R]):-  % equal degree of
         lives_in(X, CityX),                    % attractiveness, equal
         lives_in(Z, CityZ),                    % no. of common interests
         CityX == CityZ,!,                      % but X and Z live in the
         insert_in_order(X,Y,N,T,R).            % same city

insert_in_order(_,Y,N,[a(Z,M)|T],[a(Y,N),a(Z,M)|T]).  % equal degree of
                                                      % attractiveness, equal
                                                      % no. of common interests
                                                      % and X&Z do not live
                                                      % in the same city

/* get the list of preferences for a person */ 
get_preferences(Person, Preferences):-
	preferences(Person, Preferences).