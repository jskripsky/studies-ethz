%********************************************************************
  excercise info3 prolog "partners agency"

  filename:      pa.pl
  created:       2001-12-28 by andrea weisskopf <apw@trash.net>
  last modified: 2002-01-04 by andrea weisskopf <apw@trash.net>
%********************************************************************


run :-
	names(Men, Women),
	
	cities(Men, Men_cities, Women, Women_cities),

	interests(Men, Men_interests, Women, Women_interests),

	attrac(Men, Men_attrac, Women, Women_attrac),
	
	pref_list(Men, Men_attrac, Men_interests, Men_cities, Women, Women_attrac, Women_interests, Women_cities, Men_pref, Women_pref),
	
	best_pairs(Men, Women, Men_pref, Women_pref, Pairs),

	write_result(Pairs).
% -------------------------------------------------------------------------------   
word_char(C,C) :- 97 =< C, C =< 122.
word_char(C,C1) :- 65 =< C, C =< 90, C1 is C + 32.
word_char(C,C) :- 48 =< C, C =< 57.
word_char(95,95).                                      % _
word_char(39,39).                                      % '

fill_char(32).                                         % space
fill_char(40).                                         % (
fill_char(41).                                         % )
fill_char(33).                                         % !
fill_char(63).                                         % ?
fill_char(_).                                          % we treat any other "bad"
                                                       % char as a fill char
end_of_words_char(46).                                 % .
end_of_words_char(10).                                 % newline
% -------------------------------------------------------------------------------   
read_word_list(Ws) :-
	get0(C),
	read_word_list(C,Ws).

read_word_list(C,[W|Ws]) :-
	word_char(C,C1),
	read_word(C1,W,C2),
	read_word_list(C2,Ws).
read_word_list(C,[]) :-
	end_of_words_char(C).
read_word_list(C,Ws) :-
	fill_char(C),
	get0(C1),
	read_word_list(C1,Ws).

read_word(C,W,C1) :-
	word_chars(C,Cs,C1),
	name(W,Cs).

word_chars(C,[C1|Cs],C0) :-
	word_char(C,C1), !,
	get0(C2),
	word_chars(C2,Cs,C0).
word_chars(C,[],C) :-
	\+ word_char(C,_).
% -------------------------------------------------------------------------------
read_line(W) :-
	get0(C),
	read_line(C,Ws),
	name(W,Ws).

read_line(C,[]) :-
	end_of_words_char(C).

read_line(C,[C|Ws]) :-
	get0(C2),
	read_line(C2,Ws).
% -------------------------------------------------------------------------------
names(Men, Women) :-
	write('******************* NAMES *************************'), nl,
	write('The list of names consists of names (only letters)'), nl,
	write('separated by spaces and terminated by newline'), nl,
	nl,

	write('Introduce the list of men names:\t'),
	read_word_list(Men),

	write('Introduce the list of women names:\t'),
	read_word_list(Women), nl, nl, nl.
% -------------------------------------------------------------------------------
list_length([], 0).
list_length([_|Xs], N2) :-
	list_length(Xs, N1),
	N2 is (N1 + 1).
% -------------------------------------------------------------------------------
check_pairs(Men, Women) :-
	list_length(Men, M),
	list_length(Women, W),
	(\+(M = W) -> (write('fail'),!,fail);true).
% -------------------------------------------------------------------------------
read_by_person_value([], [], _).
read_by_person_value([M|Ms], [MC|MCs], T) :-
	write('\t'), write(T), write(' '), write(M), write(': '),
	read_line(MC),
	read_by_person_value(Ms, MCs, T).
% -------------------------------------------------------------------------------
cities(Men, Men_cities, Women, Women_cities) :-
	write('******************* CITIES ************************'), nl,
	write('The name of the city consists of one word'), nl,
	write('(only letters) followed by newline'), nl,
	nl,
	write('For each man input the city he lives in'), nl,
	read_by_person_value(Men, Men_cities, 'City of'),
	write('For each woman input the city she lives in'), nl,
	read_by_person_value(Women, Women_cities, 'City of'), nl, nl, nl.
% -------------------------------------------------------------------------------
read_by_person_list([], [], _).
read_by_person_list([M|Ms], [MC|MCs], T) :-
	write('\t'), write(T), write(' '), write(M), write(': '),
	read_word_list(MC),
	read_by_person_list(Ms, MCs, T).
% -------------------------------------------------------------------------------
interests(Men, Men_interests, Women, Women_interests) :-
	write('******************* INTERESTS *********************'), nl,
	write('The list of interests consists of a list of'), nl,
	write('at most 5 words (one word contains only letters),'), nl,
	write('the words being separated by spaces, and the list'), nl,
	write('being terminated by newline'), nl,
	nl,
	write('For each man input his interests'), nl,
	read_by_person_list(Men, Men_interests, 'Interests of'), nl,
	write('For each woman input her interests'), nl,
	read_by_person_list(Women, Women_interests, 'Interests of'), nl, nl, nl.
% -------------------------------------------------------------------------------
attrac(Men, Men_attrac, Women, Women_attrac) :-
	write('**************** LEVELS OF ATTRACTIVENESS **************'), nl,
	write('For each man/womenindicate the level of attractiveness for the specified woman/man'), nl,
	write('(a number in the range 1-10, 10 = most attractive followed by a newline)'), nl,
	nl,
	read_attrac(Men, Women, Men_attrac), nl,
	read_attrac(Women, Men, Women_attrac), nl,
	nl.
% -------------------------------------------------------------------------------
read_attrac([], _, []).
read_attrac([M|Ms], Women, [MA|MAs]) :-
	write('How attractive finds '), write(M), nl,
	read_attrac_for_one(Women, MA),
	read_attrac(Ms, Women, MAs).

read_attrac_for_one([], []).
read_attrac_for_one([W|Ws], [MA|MAs]) :-
	write('\t'), write(W), write(': '),
	read_number(MA),
	read_attrac_for_one(Ws, MAs).
% -------------------------------------------------------------------------------
pref_list(Men, Men_attrac, Men_interests, Men_cities,
	Women, Women_attrac, Women_interests, Women_cities,
	Men_pref, Women_pref) :-
	
	calc_pref(Women, Men_attrac, Men_interests, Women_interests, Men_cities, Women_cities, Men_pref),
	calc_pref(Men, Women_attrac, Women_interests, Men_interests, Women_cities, Men_cities, Women_pref).
% -------------------------------------------------------------------------------
calc_pref(_, [], [], _, [], _, []).
calc_pref(Women, [MA|MAs], [MI|MIs], Women_interests, [MC|MCs], Women_cities, [MP|MPs]) :-
	calc_pref_for_one(Women, MA, MI, Women_interests, MC, Women_cities, MP_unsorted),
	sort_and_elim(MP_unsorted, MP_rev),
	reverse(MP_rev, MP),
	calc_pref(Women, MAs, MIs, Women_interests, MCs, Women_cities, MPs).

calc_pref_for_one([], [], _, [], _, [], []).
calc_pref_for_one([W|Ws], [MA|MAs], MI, [WI|WIs], MC, [WC|WCs], [[MP_rating,MP_name]|MPs]) :-
	MP_name = W,
	common(MI, WI, CI),
	common([MC], [WC], CITY),
% max 99 participants
% leider hat mein geliebtes gnu prolog keinen log10 (oder hab ihn nicht gefunden und war
% zu faul um selber einen zu schreiben);
% mit welchem man diese beschraenkung leicht aufheben koennte.
	MP_rating is ((MA*1000) + (CI * 10) + (CITY)),
	calc_pref_for_one(Ws, MAs, MI, WIs, MC, WCs, MPs).
% -------------------------------------------------------------------------------
sort_and_elim(In, Out) :-
	quick_sort_nested(In, Out_pairs),
	elim_1st(Out_pairs, Out_nested),
	simplify(Out_nested, Out).

elim_1st([], []).
elim_1st([[_|X2]|Xs], [X2|Ys]) :-
	elim_1st(Xs, Ys).

simplify([], []).
simplify([[X]|Xs], [X|Ys]) :-
	simplify(Xs, Ys).

quick_sort_nested([], []).
quick_sort_nested([Median|Xs], Sorted) :-
	partition(Xs, Median, Smaller, Bigger),
	quick_sort_nested(Smaller, SmallSorted),
	quick_sort_nested(Bigger, BigSorted),
	append(SmallSorted, [Median|BigSorted], Sorted).

partition([], _, [], []).
partition([[X1|X2]|Xs], [Median1|Median2], [[X1|X2]|Smaller], Bigger) :-
	X1 =< Median1,
	partition(Xs, [Median1|Median2], Smaller, Bigger).
partition([[X1|X2]|Xs], [Median1|Median2], Smaller, [[X1|X2]|Bigger]) :-
	X1 > Median1,
	partition(Xs, [Median1|Median2], Smaller, Bigger).
% -------------------------------------------------------------------------------
intersection([], _, []).
intersection([X|Y], M, [X|Z]) :- 
	member(X, M), 
	intersection(Y, M, Z).
intersection([X|Y], M, Z) :- 
	\+ member(X, M),
	intersection(Y, M, Z).

common(X, Y, N) :-
	intersection(X, Y, Z),
	list_length(Z, N).	
% -------------------------------------------------------------------------------
direct_pairs([], [], _, _, []).
direct_pairs([M|Ms], [MP|MPs], Women, Women_pref, [O|Os]) :-
	direct_for_one(M, MP, Women, Women_pref, O),
	direct_pairs(Ms, MPs, Women, Women_pref, Os).

direct_for_one(_, _, [], [], []).
direct_for_one(Men, [Women|_], [Women|_], [[Men|_]|_], [Men, Women]).
direct_for_one(Men, [MP|MPs], [_|Ws], [_|WPs], X) :-
	direct_for_one(Men, [MP|MPs], Ws, WPs, X).
% -------------------------------------------------------------------------------
rate_solution([], _, _, _, _, 0).
rate_solution([P|Ps], Men, Women, Men_pref, Women_pref, Z) :-
	rate_for_one(P, Men, Women, Men_pref, Women_pref, X),
	rate_solution(Ps, Men, Women, Men_pref, Women_pref, Y),
	Z is X + Y.
	

rate_for_one([PM,PW], Men, Women, Men_pref, Women_pref, Z) :-
	nth(NthWomen, Women, PW),
	nth(NthMen, Men, PM),
	nth(NthMen, Men_pref, M_pref),
	nth(NthWomen, Women_pref, W_pref),
	nth(X, W_pref, PM),
	nth(Y, M_pref, PW),
	Z is X + Y.
% -------------------------------------------------------------------------------
generate_pairs([], [], []).
generate_pairs([[M,W]|Ps], Men, [W|New_Women]) :-
	take_one_out(Men, M, New_Men),
	generate_pairs(Ps, New_Men, New_Women).

take_one_out([H|T], H, T).
take_one_out([H|T], Y, [H|Z]) :- 
	take_one_out(T,Y,Z).
% -------------------------------------------------------------------------------
rate_all([], _, _, _, _, []).
rate_all([P|Ps], Men, Women, Men_pref, Women_pref, [[O,P]|Os]) :-
	rate_solution(P, Men, Women, Men_pref, Women_pref, O),
	rate_all(Ps, Men, Women, Men_pref, Women_pref, Os).

best_pairs(Men, Women, Men_pref, Women_pref, OutD) :-
	findall(X, generate_pairs(X, Men, Women), All_pairs),
	remove_ifnot_direct(All_pairs, Men, Men_pref, Women, Women_pref, Pairs_removed),
	rate_all(Pairs_removed, Men, Women, Men_pref, Women_pref, Rated),
	min_1st(Rated, [_,OutD]).
	
remove_ifnot_direct(All_pairs, Men, Men_pref, Women, Women_pref, Pairs_removed) :-
	direct_pairs(Men, Men_pref, Women, Women_pref, Direct_pairs),
	copy_if_subelem(Direct_pairs, All_pairs, Pairs_removed).
% -------------------------------------------------------------------------------
copy_if_subelem([], [], []).
copy_if_subelem(_, [], []).
copy_if_subelem([], X, X).

copy_if_subelem([C|Cs], X1, Z) :-
	copy_if_subelem(C, X1, Z1),
	copy_if_subelem(Cs, Z1, Z).
	
copy_if_subelem(C, [X|Xs], [X|Zs]) :-
	member(C, X),
	copy_if_subelem(C, Xs, Zs).
copy_if_subelem(C, [_|Xs], Z) :-
	copy_if_subelem(C, Xs, Z).
% -------------------------------------------------------------------------------
min_1st([X|Xs], Min) :- 
     min_1st(Xs, X,Min).
min_1st([], Min,Min). 
min_1st([[XN,XD]|Xs], [Min0N,Min0D],Min) :- 
    ( XN < Min0N -> 
      Min1 = [XN, XD]
    ; Min1 = [Min0N,Min0D] 
    ), 
    min_1st(Xs, Min1,Min).
% -------------------------------------------------------------------------------
write_result(Pairs) :-
	write('************* RECOMMENDED PARTNERS **************'), nl,
	write('*\tCrt.\tMan\t\tWoman\t\t*'), nl,
	write('*\tNo.\t\t\t\t\t*'), nl, 
	write('*************************************************'), nl,
	write_pairs(Pairs, 0),
	write('*************************************************'), nl.

write_pairs([], _).
write_pairs([[M,W]|Ps], N1) :-
	N2 is N1 + 1,
	write('*\t'),              
	write(N2),
	write('\t'),
	write(M),
	write('\t\t'),
	write(W),
	write('\t\t'),
	write('*'), nl, 
	write_pairs(Ps, N2).