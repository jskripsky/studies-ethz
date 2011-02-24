% -------------------------------------------------------------------------------
%   Informatik III - Programming in Prolog                          MCN  2/12/98
% -------------------------------------------------------------------------------   


%  ELIZA Example


%  ELIZA is a classic example from Artificial Intelligence. The program aims to
%  simulate a conversation between a patient and a therapist. The program
%  responds to user's input sentences by recognizing word patterns and generating
%  corresponding response patterns. The original version of ELIZA was
%  developed by Weizenbaum, "ELIZA - A Computer Program for the Study of Natural
%  Language Communication between Man and Machine", CACM Vol.9, 1966.
%  The simple version we give here is based on the example given in the
%  textbook by Sterling and Shapiro, "The Art of Prolog", pub. MIT Press.


% -------------------------------------------------------------------------------   


:- use_module(library(lists)).

run :- nl, write('Welcome to Eliza.'), nl, write('What is your problem?'), nl,
       nl, write('>> '),
	read_word_list(Input),
	eliza(Input), ! .

eliza([bye]) :-
	nl, write('Goodbye. I hope I have helped you.'), nl.
eliza(Input) :-
	pattern(Stimulus,Response),
	match(Stimulus,Dictionary,Input),
	match(Response,Dictionary,Output),
	reply(Output),
	read_word_list(Input1), !,
	eliza(Input1).

% -------------------------------------------------------------------------------   


% match(Pattern,Dictionary,Words) ->
%       Pattern matches the word list Words where matches stored in the Dictionary

match([N|Pattern],Dictionary,Target) :-
	integer(N), lookup(N,Dictionary,LeftTarget),
	append(LeftTarget,RightTarget,Target),
	match(Pattern,Dictionary,RightTarget).
match([Word|Pattern],Dictionary,[Word|Target]) :-
	atom(Word), match(Pattern,Dictionary,Target).
match([],_,[]).

% -------------------------------------------------------------------------------


%  lookup(Key,Dictionary,Value) ->
%         looks up Key in Dictionary and returns Value where Dictionary
%         is a list of (Key,Value) pairs

lookup(Key,[(Key,Value)|_],Value).
lookup(Key,[(Key1,_)|Dictionary],Value) :-
	\+ (Key = Key1),
	lookup(Key,Dictionary,Value).

% -------------------------------------------------------------------------------   


%  pattern(Stimulus,Response) ->
%          finds a pattern Response based on pattern of Stimulus

pattern([i,am,1],[how,long,have,you,been,1,?]).
pattern([1,you,2,me,3],[what,makes,you,think,i,2,you,?]).
pattern([i,like,1],[does,anyone,else,in,your,family,like,1,?]).
pattern([i,feel,1],[do,you,often,feel,that,way,?]).
pattern([1,love,2],[do,you,feel,unloved,?]).
pattern([1,X,2],[can,you,tell,me,more,about,your,X,?]) :-
	important(X).
pattern([1,me],[what,makes,you,think,that,?]).
pattern([1],[please,tell,me,more]).

% -------------------------------------------------------------------------------   


%  important(Keyword) ->
%            words considered of special importance in user sentences

important(father).
important(mother).
important(brother).
important(sister).
important(son).
important(daughter).
important(boyfriend).
important(girfriend).
important(work).
important(exams).
important(money).

% -------------------------------------------------------------------------------   


%  reply(WordList) ->
%        prints reply to user's sentence

reply([Head|Tail]) :-
	write(Head), write(' '),
	reply(Tail).
reply([]) :-
	nl, write('>> '),!.


% -------------------------------------------------------------------------------   


%  read_word_list(WordList) ->
%                 reads in user sentence as a list of words
%                 alphas are converted to lowercase
%                 user sentence terminated by `.' or return (or bad character)

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


%  characters occurring in words of user sentences
%  include digits, underscores and apostrophes

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

% -------------------------------------------------------------------------------   
