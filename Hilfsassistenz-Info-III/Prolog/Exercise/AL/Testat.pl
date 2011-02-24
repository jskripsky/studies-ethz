%File: Testat.pl
%Author: Alain Lehmann <lehmanal@student.ethz.ch>
%created: 02.01.2002

:- use_module(library(lists)).

main:-	getInput,
	write('finding solution........'),nl,
	findMatch,
	printSolution,
	nl,write('thanks for using this program'),nl.

%********************************************************************
%*************************Input reading part*************************
%Input routines.. storing the input as facts.
%	[wo]man(X)             :- List of [wo]men
%	loves(A,B,Val)         :- the relation between A and B is Val
%	interests(X,Interests) :- List of X's interests 
%	location(X)            :- The Location of X
getInput:-
	write('Geben Sie bitte die Namen der Maenner ein.'),nl,
	write('Um die Liste abzuschliessen, geben Sie "end." ein'),nl,
	getMen(Men), assert(men(Men)),
	
	write('Geben Sie nun die Namen der Frauen ein.'),nl,
	getWoman(Women,Men), assert(women(Women)),
	
	write('Alle Personen sollen sich auf einer Skala von 1-10'),nl,
	write('gegenseitig bewerten.'),nl,
	getLoveFactors,
	
	write('Alle Personen sollen eine Liste von Interessen/Hobbies'),nl,
	write('angeben. z.B.: "[cooking,swimming]."'),nl,
	getInterests,
	
	write('Alle Personen sollen ihren Wohnort angeben.'),nl,
	getLocation.
	
%-----------------Get List of men and woman---------------
getMen(Men):-
	write('Name: '),read(In),
	(In=end -> Men=[];
	 getMen(X),append([In],X,Men)
	).
getWoman([],[]).
getWoman([Woman|WomenList],[_|MenList]):-
	write('Name: '), read(Woman),
	getWoman(WomenList,MenList).
	
%-----------------Get the loveFactor of each pair.--------
getLoveFactors:-
	men(Men),women(Women),\+ getLoveMatrix(Men,Women), \+ getLoveMatrix(Women,Men).
getLoveMatrix([Pers|_],PartnerList):-getLoveArray(Pers,PartnerList).
getLoveMatrix([_|PersList],PartnerList):-getLoveMatrix(PersList,PartnerList).

getLoveArray(Person,[Partner|PartnerList]):-
	write(Person),write(' - '),write(Partner),write(': '),
	read(Value),assert(loves(Person,Partner,Value)),
	getLoveArray(Person,PartnerList).

%-----------------Get the interests of each person--------
getInterests:-
	men(Men), getInterests(Men),
	women(Women), getInterests(Women).
getInterests([]).
getInterests([Person|PersList]):-
	write(Person),write('\'s Interests: '),
	read(InList),assert(interest(Person,InList)),
	getInterests(PersList).
	
%-----------------Get the location of each person----------
getLocation:-
	men(Men), getLocation(Men),
	women(Women), getLocation(Women).
getLocation([]).
getLocation([Person|PersList]):-
	write(Person),write(' wohnt in: '),nl,
	read(Town),assert(location(Person,Town)),
	getLocation(PersList).



%********************************************************************
%****************************Solving part****************************

%returns: Element of a list and
%         the list without the element.
getNext([Choosen|Xs], Choosen, Xs).
getNext([X|Xs], Choosen, [X|Ys]):-
	getNext(Xs, Choosen, Ys).

%returns: The number of elements, which are in both lists.
listMatches([],_,0).
listMatches([X|Xs],Ys,Count):-
	listMatches(Xs,Ys,Count2),
	inList(X,Ys,Count3),
	Count is Count2+Count3.
%returns: How many times an element is in a list.
inList(_,[],0).
inList(X,[X|Xs],Count):-!,inList(X,Xs,Count2), Count is Count2+1.
inList(X,[_|Ys],Count):-inList(X,Ys,Count).

%generate the pairs.
findMatch:-men(Men),women(Women),
	findMatch(Men,Women,[]).
findMatch([], _, Perm):-assert(solution(Perm)).
findMatch([Who|WhoRest],Rest,Perm):-
	getNext(Rest,Choosen,NewRest),
	\+lovesMore(Who,Choosen,Perm),
	findMatch(WhoRest, NewRest, [[Who|Choosen] | Perm]).

%check, that the new pair doesn't want to be together with another pair.
lovesMore(NewMan,NewWoman,[[Man|Woman]|_]):-
	lovesOther(NewMan,NewWoman,Woman),
	lovesOther(NewWoman,NewMan,Man).
lovesMore(NewMan,NewWoman,[_|Rest]):-
	lovesMore(NewMan,NewWoman,Rest).
	

%which Partner does the Person loves more.	
lovesOther(Person,Partner,NewPartner):-
	loves(Person,Partner,A),
	loves(Person,NewPartner,B),
	(A<B -> true;
	 A>B -> fail;
	 lovesOtherByInterests(Person,Partner,NewPartner)
	).
lovesOtherByInterests(Person,Partner,NewPartner):-
	interests(Person,PersInter),
	interests(Partner,PartInter),
	interests(NewPartner,NewPartInter),
	listMatches(PersInter,PartInter,A),
	listMatches(PersInter,NewPartInter,B),
	(A<B -> true;
	 A>B -> fail;
	 lovesOtherByLocation(Person,Partner,NewPartner)
	).
lovesOtherByLocation(Person,Partner,NewPartner):-
	location(Person,PersLoc),
	location(Partner,PartLoc),
	location(NewPartner,NewPartLoc),
	PersLoc=\=PartLoc, PersLoc=NewPartLoc.


%********************************************************************
%*****************************Output part****************************

printSolution:-solution(PairList), 
	write('*************Pairs****************'),nl,
	printSolution(PairList),
	write('**********************************').
	
printSolution([]).
printSolution([[Man|Woman]|PairList]):-
	printSolution(PairList),
	write('**\t'),
	write(Man),write('\tand\t'),write(Woman),
	write('\t**'),nl.

