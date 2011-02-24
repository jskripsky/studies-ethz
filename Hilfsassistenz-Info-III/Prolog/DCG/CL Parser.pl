% terminal symbols
% ================
ident(I) --> [I], {atom(I)}.
number(N) --> [N], {number(N)}.

val --> [val].
equal --> [=].
set --> [set].
bag --> [bag].

insert --> [insert].
delete --> [delete].
in --> [in].

clear --> [clear].
hlt --> [halt].

operator(*) --> ['*'].
operator(+) --> ['+'].
operator(-) --> ['-'].

open_br --> ['('].
close_br --> [')'].


% non-terminal symbols
% ====================
decl(S1,S2,V) --> val, ident(I), equal, bulk(B).
bulk(set) --> set.
bulk(bag) --> bag.

ins(S1,S2,V) --> insert, number(N), in, ident(I).
del(S1,S2,V) --> delete, number(N), in, ident(I).
clr(S1,S2,V) --> clear, ident(I).

cmd(S1,S2,V) --> decl(S1,S2,V).
cmd(S1,S2,V) --> ins(S1,S2,V).
cmd(S1,S2,V) --> del(S1,S2,V).
cmd(S1,S2,V) --> clr(S1,S2,V).
cmd(S,_,V) --> expr(S,V).
cmd(_,_,_) --> hlt.

expr(S,V) --> closed_expr(S,V).
expr(S,V) --> closed_expr(S,V1), operator(O), closed_expr(S,V2).

closed_expr(S,V) --> ident(I).
closed_expr(S,V) --> br_expr(S,V).

br_expr(S,V) --> open_br, expr(S,V), close_br.


% Testing:
% ========
% phrase(cmd(_,_,_), ['(', a, '*', b, ')', '+', c]).