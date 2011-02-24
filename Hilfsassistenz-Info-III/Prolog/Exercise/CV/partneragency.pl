:- use_module(library(lists)).


starline:- write('************************************************************'), nl.	%60 stars%
starframe:- write('*                                                          *'), nl.

intro:- starline,
	starframe,
	write('*               Wellcome to Partners Agency                *'), nl,
	starframe,
	starline,
	nl.

%----------------------------------Einlesen------------------------------------------------------

manlist(ManList):- 
		starline,
		write('*               Please enter the men`s names               *'), nl,
		write('*          (separated by spaces, ended by Return)          *'), nl,
		write('***:'),
		read_word_list(ManList),
		starframe.

womanlist(WomanList):- 	
		write('*               Please enter the women`s names             *'), nl,
		write('*          (separated by spaces, ended by Return)          *'), nl,
		write('***:'),
		read_word_list(WomanList),
		starline.
		
mancities(ManList, ManCities):-
		starframe,
		write('*                 Enter the men`s cities:                  *'), nl, 
		write('*             (with a Return after each entry)             *'), nl,
		gothrough(ManList, ManCities).

womancities(WomanList, WomanCities):-
		starframe,
		write('*                Enter the women`s cities:                 *'), nl,
		write('*             (with a Return after each entry)             *'), nl,
		gothrough(WomanList, WomanCities).

interests(WomanList, ManList, WomanInt, ManInt):-
		write('*  Please enter up to five interests for each applicant,   *'), nl,
		write('*        seperated by spaces and ended by NewLine          *'), nl,
		write('*                 after each applicant.                    *'), nl,
		starframe,
		gothrough(ManList, ManInt),
		gothrough(WomanList, WomanInt).

rating(ManList, WomanList, ManRate, WomanRate):-
		write('*   For each man and woman indicate how attractive they    *'), nl,
		write('* find the other in the range 1-10. (10 = most attractive) *'), nl,
		rate(ManList, WomanList, ManRate),
		rate(WomanList, ManList, WomanRate).

rate([],_,[]):-
		write('*That`s the rating for the one side.                       *'), nl, starline.

rate([X|Xs], Ys, [Z|Zs]):-
		write('*'), write(X),
		write('`s rating: '), nl,
				gothrough(Ys, T),
				flatten(T,Z),		%nötig, weil einzelne Ratings Listen 
				rate(Xs, Ys, Zs).

gothrough([],[]):-
		write('*Thank you!                                                *'), nl, starline.

gothrough([X|Xs], [Y|Zs]):-
		write('***for '), write(X), write(': '),
		read_word_list(Y), starframe,
		gothrough(Xs, Zs).

%-----------------------------------Preferenzlisten erstellen--------------------------------------

preferencelist(ManList, WomanList, ManRate, WomanRate, ManInt, WomanInt, ManCities, 							WomanCities, ManPref, WomanPref):-
		preferences(ManList, WomanList, ManRate, ManInt, WomanInt, ManCities, WomanCities, 					ManPref),
		preferences(WomanList, ManList, WomanRate, WomanInt, ManInt, WomanCities, ManCities, 					WomanPref).

preferences([],_,_,_,_,_,_,[]).
preferences([X|Xs], Ys, [R|Rs], [I|Is], IIs, [C|Cs], CCs, Ps):-		% X wird nicht gebraucht
		bubblesort(Ys, R, I, IIs, C, CCs, P),			%Sortiere Liste fuer einen Mann/Frau
		preferences(Xs, Ys, Rs, Is, IIs, Cs, CCs, PP),		%Betrachte Rest
		append([P], PP, Ps).					%Fuege zusammen

%R: Rating des Mannes/Frau, fuer die Liste erstellt wird
%I: seine Interessen, IIs: Interessen der moeglichen Partner
%C: seine Stadt, CCs: Staedteliste    "
%Das ist ein ganz normaler BubbleSort, ausser dass ich statt der zu sortierenden Liste auch die Hilfslisten
%in gleichem Masse veraendern muss.

bubblesort(List, R, I, IIs, C, CCs, Sorted) :-
   	 	bubblesort_1(List, R, I, IIs, C, CCs, [], Sorted).

bubblesort_1([],_,_,_,_,_,Acc,Acc).
bubblesort_1([X|Xs], [Rx|Rs], I, [Ix|IIs], C, [Cx|CCs] , Acc, Sorted) :-
		aux(X,Xs, Rx, Rs, I, Ix, IIs, C, Cx, CCs, RNew, INew, CNew, Ys, Max),
    		bubblesort_1(Ys, RNew, I, INew, C, CNew, [Max|Acc], Sorted).

aux(X,[],Rx,_,_,Ix,_,_,Cx,_,Rx, Ix,Cx,[],X).
aux(X,[Y|Ys], Rx, [Ry|Rs], I, Ix, [Iy|IIs], C, Cx, [Cy|CCs],[Ry|RNew],[Iy|INew],[Cy|CNew],[Y|Zs],Max) :-
		Rx > Ry,
    		aux(X,Ys,Rx, Rs, I, Ix, IIs, C, Cx, CCs, RNew,INew,CNew,Zs,Max).
aux(X,[Y|Ys],Rx, [Ry|Rs], I, Ix, [Iy|IIs], C, Cx, [Cy|CCs], [Rx|RNew],[Ix|INew],[Cx|CNew],[X|Zs],Max) :-
    		Rx<Ry,
    		aux(Y,Ys, Ry, Rs, I, Iy, IIs, C, Cy, CCs, RNew,INew,CNew,Zs,Max).
aux(X,[Y|Ys], Rx, [Ry|Rs], I, Ix, [Iy|IIs], C, Cx, [Cy|CCs],[Ry|RNew],[Iy|INew],[Cy|CNew],[Y|Zs],Max) :-
		more_in_common(I, Ix, Iy, C, Cx),
		aux(X,Ys,Rx, Rs, I, Ix, IIs, C, Cx, CCs, RNew, INew, CNew, Zs,Max).
%Jetzt ist noch der letzte Fall mit Y>X, weil Y mehr Interessen gemeinsam hat
aux(X,[Y|Ys],Rx, [Ry|Rs], I, Ix, [Iy|IIs], C, Cx, [Cy|CCs],[Rx|RNew],[Ix|INew],[Cx|CNew],[X|Zs],Max) :-
    		aux(Y,Ys, Ry, Rs, I, Iy, IIs, C, Cy, CCs, RNew, INew, CNew, Zs,Max).

more_in_common(Is, Ixs, Iys, C, Cx):-
		more_interests(Is, Ixs, Iys).	%erste Variante, more_in_common true zu machen
more_in_common(Is, Ixs, Iys, C, Cx):-
		equal_interests(Is, Ixs, Iys),	%zweite Variante
		C=Cx.
more_interests(Is, Ixs, Iys):-
		count(Is, Ixs, X),		%count zaehlt gleiche Elemente
		count(Is, Iys, Y),
		!, X>Y.				%weiss nicht genau, weshalb Cut noetig, aber er ist's
equal_interests(Is, Ixs, Iys):-
		count(Is, Ixs, X),
		count(Is, Iys, Y),
		!, X=Y.
count([],_,0).
count([X|Xs], Ys, Sum):-			%Zerlege erste Liste und checke, ob Kopf in anderer Liste
		count(Xs, Ys, T),
		member(X, Ys),			
		Sum is T+1.			%Falls in Liste, erhoehe Sum
count([X|Xs], Ys, Sum):-
		count(Xs, Ys, Sum).		%X war nicht in Liste, deshalb Sum belassen und Rest vergl.


%--------------------------Erstellen der auf alle Faelle zu erfuellenden Paare---------------------
%				(Ich gehe immer vom Mann aus)

requested_matches([], _, _, _, []).

requested_matches([M|ManList], WomanList, [MP|ManPref], WomanPref, RMatch):-
		r_match(M, WomanList, MP, WomanPref, Match),
		requested_matches(ManList, WomanList, ManPref, WomanPref, Matches),
		append(Match, Matches, RMatch).

r_match(M, WomanList, MP, WomanPref, RMatch):-
		nth(1, MP, Whish),		%Whish: Nummer 1 Wahl von M
		nth(Pos, WomanList, Whish),	%Pos: ihre Pos in Frauenliste
		nth(Pos, WomanPref, Pref), 	%Pref: ihre Preferenzliste
		nth(1, Pref, M),!,
		RMatch = [(M, Whish)].		%Fuer append muss das eine Liste sein. Paarbildung erlaubt 							%einfacheres Testen.
r_match(M, WomanList, MP, WomanPref, []).	%oberes r_match fail => dieser Mann in keinem Paar gezwungen 


%-------------------------------Erstellen moeglicher Paarungen---------------------------------------

match_list([M|ManList], WomanList, [MP|ManPref], WomanPref, RMatch, Points, Matches):-
		try_match(M, ManList, WomanList, MP, ManPref, WomanPref, Matches, Points),
		request_check(RMatch, Matches).		%Falls nicht true, ganze Liste nicht brauchbar

try_match(Man, [], [Woman], MP, [], [WP], [(Man,Woman)], Points):-		%ein Mann => eine Frau
		nth(N1, MP, Woman),		%Paar als Rueckgabe
		nth(N2, WP, Man),
		Points is N1+N2.

try_match(M, [NextM|ManList], WomanList, MP, [NextMP|ManPref], WomanPref, Matches, Points):-
		member(X, MP),			%alle möglichen Partner werden gecheckt: Rekursion
		member(X, WomanList),		%vielleicht Frau schon vergeben
		nth(Pos, WomanList, X, NewWL),	%Pos ist die Stelle der Partnerin in der WomanList
		nth(N1, MP, X),			%N1 ist die Preferenz des Mannes fuer diese Partnerin
		nth(Pos, WomanPref, WP,NewWP),	%WP ist die Preferenzliste dieser Frau
		nth(N2, WP, M), 	%N2 ist die Stelle des Mannes auf der Preferenzliste der Frau
		try_match(NextM, ManList, NewWL, NextMP, ManPref, NewWP, AccMatches, AccPoints),
		append([(M, X)], AccMatches, Matches),
		N3 is N1+N2,			%Zus'fuegen der Teilresultate
		Points is N3 + AccPoints.

request_check([M|RMatches], Candidates):-	%RMatches: einzuhaltende Paare, Candidates: TestListe
		member(M, Candidates),		%Teste fuer jedes benoetigte Paar, ob in Liste
		request_check(RMatches, Candidates).

request_check([],_).
	
best_match(Points, MatchList, Pairs):-		%MatchList: alle erlaubten Paare, Points: ihre Guete
		max_list(Points, X),		
		nth(Pos, Points, X),		%Die zum Max gehoerende Liste ist an gleicher Stelle
		nth(Pos, MatchList, Pairs).

finish(Pairs):-
		starline,
		starframe,
		write('*               Here is the best Matching:                 *'), nl,
		starframe,
		output(Pairs),
		starframe,
		starframe,
		write('*    Thanks for jonying our Partner`s Agency. GOOD BYE!    *'), nl,
		starline,
		starline.

output([]).
output([(X,Y)|Zs]):-
		starframe,
		write('*   '), write(X), write(' and '), write(Y), nl,
		output(Zs).
		

init:- 	[readwordlist],
	[flatten].

run:- 	intro,
	manlist(ManList),
	womanlist(WomanList),
	mancities(ManList, ManCities),
	womancities(WomanList, WomanCities),
	interests(WomanList, ManList, WomanInt, ManInt),
	rating(ManList, WomanList, ManRate, WomanRate),
	write('*Please wait...                                            *'), nl,
	preferencelist(ManList, WomanList, ManRate, WomanRate, ManInt, WomanInt, ManCities, 					WomanCities, ManPref, WomanPref),
	requested_matches(ManList, WomanList, ManPref, WomanPref, RMatch),
	findall(Matches, match_list(ManList, WomanList, ManPref, WomanPref, RMatch, Points, Matches), 				MatchList),
	findall(Points, match_list(ManList, WomanList, ManPref, WomanPref, RMatch, Points, Matches), 				PointList),
	best_match(PointList, MatchList, Pairs),
	finish(Pairs).

 