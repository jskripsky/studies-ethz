
%Info 3 Testatuebung 2<
%Prolog Partner Agency
%Thierry Buecheler

:- use_module(library(lists)).

run :- nl, write('Welcome to Partners Agency.'), 
       nl, write('Enter men...'),
       nl, write('>> '),
       read_word_list(Men),
       write(Men),
       nl, write('Enter women...'),
       nl, write('>> '),       
       read_word_list(Women),
       write(Women),
       testlength(Men, Women),
       read_cities(Men),
       read_cities(Women),
       read_interests(Men),
       read_interests(Women),
       read_rankings(Men, Women),
       read_rankings(Women, Men),
       common_interests(Men, Women), 
       common_city(Men, Women),
       concat(Men, Women),   
       concat(Women, Men),
       writeout(Men),
       writeout(Women),
       writemen(Men),       
       %get0(C),
       match1.
       
       testlength(X, Y):-length(X, A), length(Y, B), A=B, !.    

% -------------------------------------------------------------------------------   
%match
        match1 :-
         
        	getman(X),         	
        	loves(X, L1),
	        loves(L1, L2),        	
        	(X=L2->match(X,L1) ;nomatch(X)), 
        	match1, !.
        	
        match1:-!.
        
	match(X, L1) :-	  	         	        
	        retractall(loves(X,A)), 
	        retractall(loves(B, X)),
	        retractall(loves(L1,C)),
	        retractall(loves(D,L1)),
	        retractall(getman(X)),
	        nl, write(X), write(' with '), write(L1).
	        
	nomatch(X) :- 
		retractall(getman(X)),
		assertz(getman(X)).
	        
       	        	 		
		
% -------------------------------------------------------------------------------   
%retract_all
	retract_all(_, []).
	retract_all(Name, [Xs|Ys]) :- 
	        retractall(loves(Name, Xs)), retract(loves(Xs, Name)),
	        retract_all(Name, Ys).
		
% -------------------------------------------------------------------------------   

% -------------------------------------------------------------------------------   
%writemen
	writemen([]).
	writemen([Xs|Ys]) :- writemen(Ys), asserta(getman(Xs)).
		
% -------------------------------------------------------------------------------   

%writeout
	writeout([]).
	writeout([Xs|Ys]) :- 
	        ranklist(Xs, Rlist),
		writeout1(Xs, Rlist), 
		writeout(Ys).
	
	writeout1(_,[]).
	writeout1(Name, [[Xs,A,B,C]|Ys]):-
		asserta(loves(Name, C)), 
		writeout1(Name, Ys). 
       
% -------------------------------------------------------------------------------   
%sorting rankinglist 1.item

        insert_sort1(List, Sorted) :-
		insert_sort1(List, [], Sorted).
	
	insert_sort1([], Acc, Acc).
	insert_sort1([X|Xs], Acc, Sorted) :-
		ord_insert1(X, Acc, Acc1),
		insert_sort1(Xs, Acc1, Sorted).
	
	ord_insert1(X, [], [X]).
	ord_insert1([Xa|Ya], [[Yc|Yd]|Ys], [[Xa|Ya],[Yc|Yd]|Ys]) :-
		Xa < Yc.
		
	ord_insert1([Xa|Ya], [[Yc|Yd]|Ys], [[Yc|Yd]|Zs]) :-
		Xa >= Yc, ord_insert1([Xa|Ya], Ys, Zs).
          
% -------------------------------------------------------------------------------   
%sorting rankinglist 2.item

        insert_sort2(List, Sorted) :-
		insert_sort2(List, [], Sorted).
	
	insert_sort2([], Acc, Acc).
	insert_sort2([X|Xs], Acc, Sorted) :-
		ord_insert2(X, Acc, Acc1),
		insert_sort2(Xs, Acc1, Sorted).
	
	ord_insert2(X, [], [X]).
	ord_insert2([Xa, C|Ya], [[Yc, E|Yd]|Ys], [[Xa, C|Ya],[Yc, E|Yd]|Ys]) :-
		C < E.
	ord_insert2([Xa, C|Ya], [[Yc,E|Yd]|Ys], [[Yc,E|Yd]|Zs]) :-
		C >= E, ord_insert2([Xa, C|Ya], Ys, Zs).

% -------------------------------------------------------------------------------   
%sorting rankinglist 3.item

        insert_sort3(List, Sorted) :-
		insert_sort3(List, [], Sorted).
	
	insert_sort3([], Acc, Acc).
	insert_sort3([X|Xs], Acc, Sorted) :-
		ord_insert3(X, Acc, Acc1),
		insert_sort3(Xs, Acc1, Sorted).
	
	ord_insert3(X, [], [X]).
	ord_insert3([Xa, B, C|Ya], [[Yc, D, E|Yd]|Ys], [[Xa, B, C|Ya],[Yc, D, E|Yd]|Ys]) :-
		C < E.
	ord_insert3([Xa, B, C|Ya], [[Yc, D, E|Yd]|Ys], [[Yc, D, E|Yd]|Zs]) :-
		C >= E, ord_insert3([Xa, B, C|Ya], Ys, Zs).

% -------------------------------------------------------------------------------   
%concat
       concat([], _).
       concat([Xs|Ys], Others):-
                 con(Xs, Others, List), 
                 insert_sort3(List, List3),
                 insert_sort2(List, List2),
                 insert_sort1(List, SortedList),
                 asserta(ranklist(Xs, SortedList)),                 
                 concat(Ys, Others).
       
       con(_, [], []).
       con(Name, [Xs|Ys],Lout):- 
                 rank(Name, Xs, R), 
                 common_int(Name, Xs, I),
                 com_city(Name, Xs, C),                 
                 con(Name, Ys, Out),
                 append([[R, I, C, Xs]], Out, Lout).
                 

       
% -------------------------------------------------------------------------------   
%read common cities
       common_city([],_).
       common_city([Xs|Ys], Others):-
       		com_c(Xs, Others), 
       		common_city(Ys, Others).
       
       com_c(_,[]).
       com_c(Name, [Xs|Ys]):-
                city(Name, C1), 
                city(Xs, C2),
                (C1=C2 -> asserta(com_city(Name, Xs, 1)), asserta(com_city(Xs, Name, 1)); 
                asserta(com_city(Name, Xs, 0)), asserta(com_city(Xs, Name, 0))),
                com_c(Name, Ys).

% -------------------------------------------------------------------------------   
%read cities
       read_cities([]).
       read_cities([Xs|Ys]):-
                nl, write('Enter city of '),write(Xs),write('...'),
       		nl, write('>> '),
       		read_word_list([I|F]),
       		nl, write(I), 
       		asserta(city(Xs, I)), 
       		read_cities(Ys).

% -------------------------------------------------------------------------------   
%read interests
      common_interests([],_).
      common_interests([Xs|Ys], Others):-
                com_in(Xs, Others), common_interests(Ys, Others).
                
      com_in(_, []).          
      com_in(Name, [Xs|Ys]):-
      		interests(Name, I1), 
      		interests(Xs, I2), 
      		delete1(I1, I2, D), 
      		length(I2, A),
      		length(D, B),
      		Ci is A-B,
      		asserta(common_int(Name, Xs, Ci)),
      		asserta(common_int(Xs, Name, Ci)),
      		com_in(Name, Ys). 
      		
% -------------------------------------------------------------------------------   

     delete1([], A, A).
     delete1([A|B], C, D) :-
		delete1(B, C, E), remove(A, E, D).
		
     remove(A, [A|B], B).
     remove(A, [B|C], D) :-
		remove(A, C, E), append([B], E, D).
     remove(_, [], []).
		
% -------------------------------------------------------------------------------   
%read interests
       
       read_interests([]).
       read_interests([Xs|Ys]) :- 
       		nl, write('Enter interests of '),write(Xs),write('...'),
       		nl, write('>> '),
       		read_word_list(I),       		       		
       		nl, write(I),
       		asserta(interests(Xs, I)), 
       		read_interests(Ys).
       		       
% -------------------------------------------------------------------------------   
%read ranking
     	  
       read_rankings([], _).
       read_rankings([Xs|Ys], Rankers) :- 
                rank_every(Xs, Rankers, Ranks),
                asserta(rankings(Xs, Ranks)),
                nl, write(Xs), write(' ranks: '),
                nl, write(Ranks),
       		read_rankings(Ys, Rankers).
       		
       rank_every(_, [],[]).
       rank_every(Name, [Xs|Ys], Out):- 
                nl, write(Name), write(' ranks '),
                write(Xs), write(' as...'),
       		nl, write('>> '),
       		read_word_list([I|F]),
       		asserta(rank(Name, Xs, I)),
       		append([Xs], [I], A),           	   		  
       		rank_every(Name, Ys, Out1),
       		append([A], Out1, Out).     		
%--------------------------------------------------------------------------------   
%read number



       		
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
word_char(C,C1) :- 65 =< C, C =< 90, C1 is C.
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
