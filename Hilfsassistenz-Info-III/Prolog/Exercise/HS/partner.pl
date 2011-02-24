% Exercise 2 - Partners Agency in Prolog
% by Hinnerk Spindler

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MAIN PROGRAM                                                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------------------------------------------------
% run/0
% -------------------------------------------------------------------
% Argument(s): 	none
% Result(s): 	none
% -------------------------------------------------------------------
% Main program, queries the user for input, checks the input,
% generates the preference lists, calculates the best match and then
% prints the result
% -------------------------------------------------------------------

run :- 
	intro, 
	read_men(Men), 
	read_women(Women),
	\+ check_size(Men, Women),
	nl, write('Please enter the cities the men/women live in:'), nl,
	read_cities(Men, CitiesMen), read_cities(Women, CitiesWomen),
	nl, write('Please enter at most 5 interests for each person:'), nl,
	read_interests(Men, InterestsMen), read_interests(Women, InterestsWomen),
	nl, write('Please enter for each man how attractive he finds each'),
	nl, write('woman by giving a number in the range 1-10.'), nl,
	read_ratings(Men, Women, RatingsMen),
	nl, write('Please enter for each woman how attractive she finds each'),
	nl, write('man by giving a number in the range 1-10.'), nl,
	read_ratings(Women, Men, RatingsWomen),
	combine_data(Men, RatingsMen, InterestsMen, CitiesMen, DataMen),
	combine_data(Women, RatingsWomen, InterestsWomen, CitiesWomen, DataWomen),
	preferences(DataMen, DataWomen, X),
	preferences(DataWomen, DataMen, Y),
	filter_names(X, PrefMen), filter_names(Y, PrefWomen),
	make_pairlists(Men,Women,PrefMen,PrefWomen,PairLists), 
	best_pairs(PairLists,_,BestPairs),
	print_result(BestPairs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% STEP 1: DATA INPUT                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% For data input i used some of the predicates from the eliza example in the     %%%
%%% lecture. Those predicates are described below in the auxiliary section.        %%%
%%% The result of step 1 are four lists for each sex with all the required data    %%%
%%% for the next steps.                                                            %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------------------------------------------------
% intro/0
% -------------------------------------------------------------------
% Argument(s): 	none
% Result(s): 	none
% -------------------------------------------------------------------
% Just prints an introduction
% -------------------------------------------------------------------

intro :-
	nl, write('***************************************************************'),
	nl, write('* Partners Agency                                             *'),
	nl, write('* written by Hinnerk Spindler                                 *'),
	nl, write('***************************************************************'),
	nl.

% -------------------------------------------------------------------
% read_men/1
% -------------------------------------------------------------------
% Argument(s): 	none
% Result(s):	Men - List of the names of the male partners
% -------------------------------------------------------------------
% Reads a list of names, using the read_list predicate.
% -------------------------------------------------------------------
	
read_men(Men) :-
	nl, write('Enter a list of men seeking partners, separated by spaces and'),
	nl, write('finish input by pressing enter'), nl, write('> '),
	read_list(Men).
	
% -------------------------------------------------------------------
% read_women/1
% -------------------------------------------------------------------
% Argument(s): 	none
% Result(s):	Women - List of the names of the female partners
% -------------------------------------------------------------------
% Reads a list of names, using the read_list predicate.
% -------------------------------------------------------------------

read_women(Women) :-
	nl, write('Enter a list of women seeking partners, separated by spaces and'),
	nl, write('finish input by pressing enter. There must be the same number'), 
	nl, write('of women as of men!'),
	nl, write('> '),
	read_list(Women).

% -------------------------------------------------------------------
% read_men/2
% -------------------------------------------------------------------
% Argument(s): 	[Name|TailN] - List of names
% Result(s):	[City|TailC] - List of cities
% -------------------------------------------------------------------
% Takes a list of names and queries the user to input a city that
% belongs to each name; the cities are returned as list.
% -------------------------------------------------------------------

read_cities([], []).
read_cities([Name|TailN], [City|TailC]) :-
	write(Name), write(' lives in: '), 
	get0(CurChar), read_word(CurChar, Cs, _),
	name(City, Cs),
	read_cities(TailN, TailC).	

% -------------------------------------------------------------------
% read_interests/2
% -------------------------------------------------------------------
% Argument(s): 	[Name|TailN] - List of names
% Result(s):	[Interests|TailI] - List of lists of interests
% -------------------------------------------------------------------
% Takes a list of names and queries the user to input up to five
% interests for each name; these are returned as list of lists.
% -------------------------------------------------------------------

read_interests([], []).
read_interests([Name|TailN], [Interests|TailI]) :-
	write(Name), write(' has the following interests: '),
	read_list(Interests),
	read_interests(TailN, TailI).
	
% -------------------------------------------------------------------
% read_ratings/3
% -------------------------------------------------------------------
% Argument(s): 	[Name|TailN] - List of names
%		List2 - List of the names of the other sex
% Result(s):	[Ratings|TailR] - List of list of ratings
% -------------------------------------------------------------------
% Takes two lists of names, loops through the first list and queries
% the user for each name of the first list to rate each name in the
% second list; result is returned as a list of lists.
% -------------------------------------------------------------------

read_ratings([], _, []).
read_ratings([Name|TailN], List2, [Ratings|TailR]) :-
	write('Enter how attractive '), write(Name),
	write(' finds each of the following:'), nl,
	read_rating(Name, List2, Ratings),
	read_ratings(TailN, List2, TailR).
	
% -------------------------------------------------------------------
% read_rating/3
% -------------------------------------------------------------------
% Argument(s): 	Name - The current person that rates
%		[Other|TailO] - List of the names of the other sex
% Result(s):	[Rating|TailR] - List of numbers (1-10)
% -------------------------------------------------------------------
% Loops through the second list and queries the user to input a
% number for each name in it; returns the numbers as a list
% (that will be combined to a list of lists)
% -------------------------------------------------------------------

read_rating(_, [], []).
read_rating(Name, [Other|TailO], [Rating|TailR]) :-
	write(Other), write(': '), read_number(Rating),
	1 =< Rating, Rating =< 10,
	read_rating(Name, TailO, TailR).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% STEP 2: GENERATING PREFERENCE LISTS                                            %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% In this step the preference lists are generated: my base idea was taking the   %%%
%%% list of names of one sex and sorting it according to the ratings etc. of each  %%%
%%% person of the other sex. I used the quicksort algorithm and adapted it to do   %%%
%%% this, which meant to carry around all the extra data, and changing the greater %%%
%%% and lesser predicates. To reduce the number of arguments, i introduced two     %%%
%%% predicates (combine_data & filter_names), to combine the 4 input lists into    %%%
%%% one list for sorting, and then after sorting to get just a list of lists of    %%%
%%% names: the preference list for each person.                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------------------------------------------------
% combine_data/5
% -------------------------------------------------------------------
% Argument(s): 	[Name|TailN] - List of names
%		[Rating|TailR] - List of the names' rating of the
%			other sex
%		[Ints|TailI] - List of the names' interests
%		[City|TailC] - List of the names' cities
% Result(s):	[[...]|TailD] - List of "records" for each name
%		Each record has the following structure:
%		[Name, Ratings, Interests, City], where Ratings
%		are Name's ratings of each of the persons of
%		the other sex.
% -------------------------------------------------------------------
% Takes the 4 lists from step 1 and reduces them to one list. This
% happens only, because i hated a "partition" predicate with about
% 12 arguments ;-)
% -------------------------------------------------------------------

combine_data([],_,_,_,[]).
combine_data([Name|TailN],[Rating|TailR],[Ints|TailI],[City|TailC],
  [[Name,Rating,Ints,City]|TailD]) :-
	combine_data(TailN,TailR,TailI,TailC,TailD).
	
% -------------------------------------------------------------------
% filter_names/2, filter_name/2
% -------------------------------------------------------------------
% Argument(s): 	[Data|TailD] - List of lists of "records"
% Result(s):	[NameList|TailN] - List of names (preflist)
% -------------------------------------------------------------------
% Undoes the combine_data operation, returning a list of names for
% step 3
% -------------------------------------------------------------------

filter_names([],[]).
filter_names([Data|TailD], [NameList|TailN]) :- 
	filter_name(Data, NameList),
	filter_names(TailD, TailN).
	
filter_name([], []).
filter_name([[Name,_,_,_]|TailD], [Name|TailN]) :-
	filter_name(TailD, TailN).

% -------------------------------------------------------------------
% preferences/3
% -------------------------------------------------------------------
% Argument(s): 	[[...]|TailD] - List of "records" of one sex
%		OtherData - List of "records" of the other sex
% Result(s):	[Pref|TailP] - List of lists of records:
%		The input for predicate "filter_names".
% -------------------------------------------------------------------
% Generates for each entry in the first list the preference list,
% which in fact means each time sorting "OtherList" according to the
% rating R, the common interests and the same city. The result thus
% is a list of those sorted "OtherLists", which themselves are lists
% of "records".
% -------------------------------------------------------------------

preferences([],_,[]).
preferences([[_,R,I,C]|DataList], OtherData, [Pref|PrefList]) :-
	quick_sort(OtherData, R, I, C, Pref),
	preferences(DataList, OtherData, PrefList).

% -------------------------------------------------------------------
% quick_sort/5
% -------------------------------------------------------------------
% Argument(s): 	[MedianData|TailD] - List of "records" to be sorted
%		[MedianR|TailR] - List of the ratings for the names
%			in the list to be sorted.
%		Ints - Interests of the person whose preference list
%			is to be sorted.
%		City - The city of that person.
% Result(s):	Sorted - The sorted list of "records".
% -------------------------------------------------------------------
% Quick_sort is called for each name whose preferences should be
% evaluated. for this i just do a standard quicksort on the list of
% the persons of the other sex. i however additionally need the
% ratings, interests and the city as an argument, to use it in the 
% "partition" predicate for comparison. the changes i made to the
% quick_sort predicate here are, that there are two lists sorted at
% the same time: the Data and the ratings, however only the data
% will be returned.
% -------------------------------------------------------------------

quick_sort([],_,_,_,[]).
quick_sort([MedianData|TailD], [MedianR|TailR], Ints, City, Sorted) :-
   partition(TailD, MedianData, TailR, MedianR, Ints, City, SmallerData, SmallerR, 
     BiggerData, BiggerR),
   quick_sort(SmallerData, SmallerR, Ints, City, SmallSorted),
   quick_sort(BiggerData, BiggerR, Ints, City, BigSorted),
   append(SmallSorted, [MedianData|BigSorted], Sorted).

% -------------------------------------------------------------------
% partition/10
% -------------------------------------------------------------------
% Argument(s): 	[X|Xs] - List of "records" to be partitioned
%		MedianData - Median of the "record"-list
%		[R|Rs] - List of ratings to be partitioned
%		MedianR - Median of the rating list
%		Ints - Interests of the person whose preference list
%			is to be sorted.
%		City - The city of that person.
% Result(s):	SmallerData - List of all "records" =< MedianData
%		SmallerR - List of all ratings =< MedianR
%		BiggerData - List of all "records" > MedianData
%		BiggerR - List of all ratings > MedianR
% -------------------------------------------------------------------
% Partitions the two lists according to the Medians
% -------------------------------------------------------------------

partition([], _, _, _, _, _, [], [], [], []).

partition([X|Xs], MedianData, [R|Rs], MedianR, Ints, City, [X|SmallerData], [R|SmallerR],
 BiggerData, BiggerR) :-
   greater(X, MedianData, R, MedianR, Ints, City),
   partition(Xs, MedianData, Rs, MedianR, Ints, City, SmallerData, SmallerR, 
     BiggerData, BiggerR).

partition([X|Xs], MedianData, [R|Rs], MedianR, Ints, City, SmallerData, SmallerR,
 [X|BiggerData], [R|BiggerR]) :-
   lesser(X, MedianData, R, MedianR, Ints, City),
   partition(Xs, MedianData, Rs, MedianR, Ints, City, SmallerData, SmallerR, 
     BiggerData, BiggerR).

% -------------------------------------------------------------------
% greater & lesser/6
% -------------------------------------------------------------------
% Argument(s): 	I - Interests of the current person to be compared
%		C - City of the current person to be compared
%		IM, CM - Interest & city of the Median
%		R - Rating of the current person
%		MedianR - Rating of the Median
%		Ints - Interests of the person whose preference list
%			is to be sorted.
%		City - The city of that person.
% Result(s):	none
% -------------------------------------------------------------------
% First the number of interests in common is calculated. The cut is
% needed, else Prolog will try satisfy compare_list again, with
% other numbers, instead of unifying with "lesser". after calculating
% the numbers of interests in common, everything is compared,
% according to the given rules: first the rting, second the interests
% last the city.
% -------------------------------------------------------------------

greater([_,_,I,C], [_,_,IM,CM], R, MedianR, Ints, City) :-
	(compare_list(I, Ints, N1),
	compare_list(IM, Ints, N2)), !,
	greater1(R, MedianR, N1, N2, CM, C, City).
	
greater1(R, MedianR, _, _, _, _, _) :-
	R > MedianR.
greater1(R, MedianR, N1, N2, _, _, _) :-
	R = MedianR,
	N1 > N2.
greater1(R, MedianR, N1, N2, CM, C, City) :-
	R = MedianR,
	N1 = N2,
	\+ CM = City,
	C = City.

lesser(_, _, R, MedianR, _, _) :-
	R < MedianR.
lesser([_,_,I,_], [_,_,IM,_], R, MedianR, _, _) :-
	R = MedianR,
	(compare_list(I, Ints, N1),
	compare_list(IM, Ints, N2)), !,
	N1 =< N2.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% STEP 3: FINDING BEST MATCH                                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% i have thought quite a lot about this step and i didn't find a solution that   %%%
%%% uses the given condition in the exercise. i however found another condition,   %%%
%%% which i think satisfies the problem too:                                       %%%
%%% first i create a list of all possible solutions for pairing the persons.       %%%
%%% now each of the pairings is given a value, according to the following rule:    %%%
%%% if A and B are a pair (and there are N persons per sex), then A gets N points, %%%
%%% if B is the first entry in A's preference list, N-1 if B is the second, and so %%%
%%% on. The same is calculated for B and the sum of both values is the pair-value. %%%
%%% Thus if its a perfect match, meaning B is the most wanted for A, and A is the  %%%
%%% same for B, the pair-value would be 2*N. This is calculated for each pair      %%%
%%% and the sum of those pair-values is the pairlist-value, and of course the      %%%
%%% pairlist with a maximum value is the solution.                                 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------------------------------------------------
% pairlist_value & pair_value/6
% -------------------------------------------------------------------
% Argument(s): 	[NameM|NameW] - Pair to be rated
%		NameMList - List of men
%		NameWList - List of women
%		PrefMList - List of men's preferences
%		PrefWList - List of women's preferences
%		[Pair|TailP] - List of Pairs
% Result(s):	N - pair-value respectivly pairlist-value
% -------------------------------------------------------------------
% Returns the value for a given pair. first gets the preference list
% for the man and the woman in the pair. then calculates L, the 
% number of persons of each sex. now, the position of the man in
% the woman's preference list and vice versa is calculated. The
% N is the sum of the two values.
% pairlist-value walks through a list of pairs and calculates the
% sum of all pair-values of that list.
% -------------------------------------------------------------------

pairlist_value([],_,_,_,_,0).
pairlist_value([Pair|TailP], NameMList, NameWList, PrefMList, PrefWList, N) :-
	pair_value(Pair, NameMList, NameWList, PrefMList, PrefWList, N1),
	pairlist_value(TailP, NameMList, NameWList, PrefMList, PrefWList, N2),
	N is N1+N2.
	
pair_value([NameM, NameW], NameMList, NameWList, PrefMList, PrefWList, N) :-
	get_pref(NameM, NameMList, PrefMList, PrefM),
	get_pref(NameW, NameWList, PrefWList, PrefW),
	length(NameMList,L),
	(pos_in_list(NameM, PrefW, N1),
	pos_in_list(NameW, PrefM, N2)),
	N is (L+1-N1)+(L+1-N2), N>0.
	
% -------------------------------------------------------------------
% make_pairlists/5
% -------------------------------------------------------------------
% Argument(s): 	NameMList - List of men
%		NameWList - List of women
%		PrefMList - List of men's preferences
%		PrefWList - List of women's preferences
% Result(s):	PairLists - List of lists of pairs, all possible
%			combinations of the men and women
% -------------------------------------------------------------------
% Creates a list of all possible pairings, by first permuting the
% men's names and putting all permutations in a permutation list, and
% then calling make1.
% -------------------------------------------------------------------

make_pairlists(NameMList, NameWList, PrefMList, PrefWList, PairLists) :-
	setof(X,perm(NameMList,X),NameMPerms),
	make1(NameMPerms,NameMList,NameWList,PrefMList,PrefWList,PairLists).
	
% -------------------------------------------------------------------
% make1/6
% -------------------------------------------------------------------
% Argument(s): 	[MPerm|TailN] - List of Permutations of list of men
%		NameMList - List of men
%		NameWList - List of women
%		PrefMList - List of men's preferences
%		PrefWList - List of women's preferences
% Result(s):	[[N,Pairlist|TailP] - List of lists of pairs, all 
%			possible combinations of the men and women
%			where N is the pairlist-value of that
%			combination
% -------------------------------------------------------------------
% Takes each permutation, makes a pairlist out of it, and then
% calulates its value. The result is a list of a list containing
% a pairlist and the respective pairlist-value.
% -------------------------------------------------------------------

make1([],_,_,_,_,[]).
make1([MPerm|TailN],NameMList,NameWList,PrefMList,PrefWList,[[N,PairList]|TailP]) :-
	make_pairlist(MPerm, NameWList, PairList),
	pairlist_value(PairList, NameMList, NameWList, PrefMList, PrefWList, N),
	make1(TailN, NameMList,NameWList,PrefMList,PrefWList, TailP).		

% -------------------------------------------------------------------
% make_pairlist/3
% -------------------------------------------------------------------
% Argument(s): 	[Man|TailM] - Permutation of List of men
%		[Woman|TailW] - List of women
% Result(s):	[[Man,Woman]|TailP] - List of pairs
% -------------------------------------------------------------------
% Creates a list of pairs from the given men's and women's names
% -------------------------------------------------------------------

make_pairlist([],[],[]).
make_pairlist([Man|TailM],[Woman|TailW],[[Man,Woman]|TailP]) :-
	make_pairlist(TailM, TailW, TailP).
	
% -------------------------------------------------------------------
% best_pairs/3
% -------------------------------------------------------------------
% Argument(s): 	[[X,P]|_] - List of evaluated pairlists
% Result(s):	P - Best pairlist
%		M,X - Maximum value
% -------------------------------------------------------------------
% Finds the pairlist with a maximum value.
% -------------------------------------------------------------------

best_pairs([], 0, []).
best_pairs([[X,P]], X, P).
best_pairs([[X,_]|Tail], M, P) :-
	best_pairs(Tail, M, P),
	M >= X, !.
best_pairs([[X,P]|_], X, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% STEP 4: RESULT OUTPUT                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------------------------------------------------
% print_result, print_line/1
% -------------------------------------------------------------------
% Argument(s): 	Best_Pairs - Pairlist
% Result(s):	none
% -------------------------------------------------------------------
% Prints the best matches
% -------------------------------------------------------------------

print_result(Best_Pairs) :-
	nl, write('***************************************************************'),
	nl, write('* The best pairs are:                                         *'),
	nl, write('***************************************************************'),	
	print_line(Best_Pairs),
	nl, write('***************************************************************'),
	nl.
	
print_line([]).
print_line([[Man,Woman]|Tail]) :-
	nl, write('   '),write(Man),write(' - '),write(Woman),
	print_line(Tail).		
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AUXILIARY PREDICATES AND TEST-ROUTINE                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% I won't comment everything here, sorry ;-)                                     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Testroutine, i didn't want to reenter everything
test(X,Y) :-
	combine_data([alex,seba,joe],[[7,5,5],[3,8,7],[6,3,7]],
	  [[x,y,z],[x,y],[h,z]],[a,b,a], D1), write(D1), nl,
	combine_data([ute,anne,melli],[[7,6,5],[8,9,6],[6,5,7]],
	  [[x,a,z],[h,y],[h,z]],[c,c,a], D2), write(D2), nl,
	preferences(D1, D2, X1), 
	preferences(D2, D1, Y1),
	filter_names(X1, X), filter_names(Y1, Y), write(X),nl, write(Y),nl,
	make_pairlists([alex,seba,joe],[ute,anne,melli],X,Y,P), 
	best_pairs(P,N,Best), print_result(Best),nl.
	  
% Checks if same number of men and women	  
check_size(List1, List2) :-
	\+ equal_size(List1, List2),
	write('Error: Different number of men and women! Program will stop.'), nl.
	
% Checks if two lists have the same number of members
equal_size([],[]).
equal_size([_|Tail1], [_|Tail2]) :- equal_size(Tail1, Tail2).

% Appends two lists
append([], L, L).
append([H|L1],L2,[H|L3]) :- append(L1, L2, L3).

% Checks if X is a member of a List
member(X, [X|_]).
member(X, [_|Tail]) :- member(X, Tail).

% Returns the number of elements two lists have in common
compare_list([],_,0).
compare_list([Head1|Tail1], L2, N) :-
	member(Head1, L2),
	compare_list(Tail1, L2, N1),
	N is N1+1.
compare_list([_|Tail1], L2, N) :-
	compare_list(Tail1, L2, N).
	
% Gets the preference list of a person out of the list of preferencelists
get_pref(Name, [Name|_], [Pref|_], Pref).
get_pref(Name, [X|TailN], [P|TailP], Pref) :- 
	get_pref(Name, TailN, TailP, Pref).
	
% Returns the size of a list	
length([], 0).
length([Head|Tail], N) :-
	length(Tail, N1),
	N1 is N+1.
	
% Returns the position of an element in a list	
pos_in_list(X, [], -1000).			% should never happen!
pos_in_list(X, [X|_], 1).
pos_in_list(X, [H|Tail], N) :-
	pos_in_list(X, Tail, N1),
	N is N1+1.

% Permutates a list - taken from the SORT.PL file
perm([],[]).
perm([X|Xs],Ys) :-
    perm(Xs,Zs),
    insert(X,Zs,Ys).

insert(X,Xs,[X|Xs]).
insert(X,[Y|Ys],[Y|Zs]) :-
    insert(X,Ys,Zs).

% -------------------------------------------------------------------
% Converting user-input to list of words
% -------------------------------------------------------------------
end_of_input(10).					% return
separator(32).						% space
word_char(C,C) :- 97 =< C, C =< 122.			% a-z
word_char(C,C) :- 65 =< C, C =< 90.			% A-Z
int_char(C,C) :- 48 =< C, C =< 57.			% 0-9

% -------------------------------------------------------------------
% read_list/1 or /2
% -------------------------------------------------------------------
% Argument: CurChar - Last Char entered by user
% Result: WordList - List of words the user entered
% -------------------------------------------------------------------

read_list(WordList) :-
	get0(CurChar),
	read_list(CurChar, WordList).
	
read_list(CurChar, [Word|WordList]) :-
	word_char(CurChar, CurChar1),				% user entered a word_char?
	read_word(CurChar1, Cs, CharAfterWord),		% start reading word
	name(Word, Cs),								% convert ASCII to name
	read_list(CharAfterWord, WordList).			% process next word
	
read_list(CurChar, []) :-						% user finished input
	end_of_input(CurChar).
	
read_list(CurChar, WordList) :-					% handle spaces between words
	separator(CurChar),
	get0(NextChar),
	read_list(NextChar, WordList).
	
read_word(CurChar, [CurChar1|Word], CharAfterWord) :-
	word_char(CurChar, CurChar1), !, 			% while the user enters word_chars
	get0(NextChar),								% concatenate them to a word
	read_word(NextChar, Word, CharAfterWord).

read_word(CurChar, [], CurChar) :-				% not a word_char? then word is
	\+ word_char(CurChar, _).					% finished

% reads a number
read_int(N, CurChar, N) :- end_of_input(CurChar).
read_int(N, CurChar, N2) :-
	int_char(CurChar, CurChar),
	D is CurChar-48,
	get0(NextChar),
	N1 is N*10+D,
	read_int(N1, NextChar, N2).
	
read_number(N) :-
	get0(CurChar), read_int(0, CurChar, N).
