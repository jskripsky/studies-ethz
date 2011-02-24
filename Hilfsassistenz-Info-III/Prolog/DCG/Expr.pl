expr(Z) --> term(X), "+", expr(Y), {Z is X+Y}.
expr(Z) --> term(Z).

term(Z) --> number(Z).

number(X) --> [C], {"0"=<C, C=<"9", X is C-"0"}.
