/********************* stable.pl ************************************/
/** algorithm for determining a stable matching                 *****/
/** given a list of preferences of each person for each person  *****/
/** of the opposite sex                                         *****/
/********************************************************************/
/**** Author: Claudia Ignat **** Date: 6.12.2001 ********************/
/********************************************************************/

:- module(stable, [stable/3]).

:-use_module(library(lists)).
:-use_module(utils, [check/3]).
:-use_module(reading, [get_preferences/2]).

/* stable determines a set of stable */
/* Marriages between the Men and the Women. */
stable(Men, Women, Marriages):-
  generate(Men, Women, Marriages),
  test(Men, Women, Marriages).

/* generate generates a set of */
/* possible marriages between the Men and the Women. */
generate([], [], []).
generate([Man|Men], Women, [marriage(Man,Woman)|Marriages]):-
  select(Woman, Women, Women1),         % select a Woman to be pair for Man
                                        % in case of failure select backtracks
  generate(Men, Women1, Marriages).     % generate the other couples

/* test succeeds if Marriages is a set of stable */
/* marriages between the Men and the Women. */
test([], _, _).
test([Man|Men], Women, Marriages):-
  test1(Women, Man, Marriages),
  test(Men, Women, Marriages).

/* check for a certain Man if it can be a source */
/* of unstable marriages */
test1([], _, _).
test1([Woman|Women], Man, Marriages):-
  \+ unstable(Man, Woman, Marriages),
  test1(Women, Man, Marriages).

/* unstable is true if the Man and the Woman */
/* prefer each other to their Wife, respectively */
/* Husband according to what is defined by the  */
/* set of Marriages.*/
unstable(Man, Woman, Marriages):-
  is_married(Man, Wife, Marriages),
  is_married(Husband, Woman, Marriages),
  prefers(Man, Woman, Wife),
  prefers(Woman, Man, Husband).

/* is_married is true if the Man */
/* and the Woman are married */
is_married(Man, Woman, Marriages):-
  check(marriage(Man, Woman), Marriages, _).

/* prefers is true if Person prefers the */
/* Other to his Spouse. */
prefers(Person, Other, Spouse):-
  get_preferences(Person, Preferences),      % predicate asserted in reading
                                         % module
  check(Other, Preferences, Rest),       % check if Other is before Spouse
  check(Spouse, Rest, _).                % in the list of Preferences
