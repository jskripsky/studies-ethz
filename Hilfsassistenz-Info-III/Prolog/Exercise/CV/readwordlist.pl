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
word_char(C,C) :- 97 =< C, C =< 122.
word_char(C,C1) :- 65 =< C, C =< 90, C1 is C + 32.
word_char(C,C) :- 48 =< C, C =< 57.
word_char(95,95).                                      % _
word_char(39,39).                                      % '

%  symbols considered as word separators
fill_char(32).                                         % space
fill_char(40).                                         % (
fill_char(41).                                         % )
fill_char(33).                                         % !
fill_char(63).                                         % ?
fill_char(_).                                          % we treat any other "bad"
                                                       % char as a fill char

%  sentences ended by `.` or return
end_of_words_char(46).                                 % .
end_of_words_char(10).                                 % newline

