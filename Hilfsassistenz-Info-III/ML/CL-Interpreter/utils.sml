(* Utility functions *)

(* Test membership in a list *)
infix mem;
fun 	(x mem []) = false
	|(x mem (y::l)) = 
		(x=y) 
		orelse (x mem l);

(* Insert a new element to a list, only if new *)
fun newmem(x,xs) = 
	if x mem xs then xs
	else x::xs;

(* Delete one occurence of a given element from a list *)
fun	del(x,[]) = []
|	del(x, hd::tl) = if (x=hd) then tl else hd::(del(x,tl));

(* Eliminates repeated elements in a list *)
fun 	setof [] = []
	|setof (x::xs) = newmem ( x , setof xs);

(* Prints a list of strings *)
fun 	printStringList ( [] : string list ) =  TextIO.print "\n"
	|printStringList ( xs::xe : string list ) =
		let
			val out=TextIO.print (xs^",")
		in
			printStringList(xe)
		end;
	
(* to work with opts *)
fun 	deopt (SOME n) = n;

(* min/max *)
fun	max(a,b) = if a>b then a else b;
fun	min(a,b) = if a<b then a else b;

(* count the number of times an element is present in a list *)
fun	count2(_,[],tot)= tot
	|count2( a : int , cs::ce : int list , tot : int) = 
		if cs=a then count2(a,ce,tot+1)
		else count2(a,ce,tot);

fun	count ( a : int , c : int list) = count2(a,c,0) : int;

(* returns all the elements of the two lists, removing the duplicates *)
fun	singleList(l1,l2) = setof(l1 @ l2);


(* returns the first char of the string *)
fun	stringToChar ( s : string) =
		List.nth(String.explode(s),0);

(* convert the string to an integer *)
fun	stringToInt (s : string)=	
		deopt(Int.fromString(s));

(* split the brackets ( ) from the list of tokens 
	Example:
		- fineTokenize(["a*(b-c)","-a"]);
		val it = ["a","*","(","b","-","c",")","-","a"] : string list
   	this works because it exploits the fact that the collection name, brackets and operators are all
	only one character
*)
fun 	fT2( "" : string) = [""]
	|fT2( s : string) = 	
		map Char.toString (String.explode s);

fun 	fineTokenize ([] : string list) = []
	|fineTokenize ( ss::se : string list) = 
		fT2(ss) @ fineTokenize(se);

(* converts a list of strings to a list of chars *)
fun	listToChar( [] : string list ) = [] : char list
	|listToChar( xs::xe : string list ) =
		if String.size(xs)>1 then List.nth(String.explode(xs),0)::listToChar(String.extract(xs,1,NONE)::xe)
		else List.nth(String.explode(xs),0)::listToChar(xe);

(* tokenizer functions *)
fun 	isChar (c : char) (d : char) =
		if d=c then true
		else false;

(* apply the tokenizer character to all the strings in the list *)
(* returns a list of strings *)
fun 	tokenize3 ( c : char , [] : string list)=[]
	|tokenize3 ( c : char, ss::se : string list)=
		(String.tokens (isChar c) ss) @ tokenize3(c,se);

(* Tokenize the list of strings using the list of separator chars *)
fun 	tokenize2 ([] : char list, s : string list) = s : string list
	|tokenize2 (cs::ce : char list, s : string list) =
		tokenize2(ce,tokenize3 (cs, s));

(* take a list of separators characters and a string *)
(* returns a list of string, tokenized using the characters *)
(* Example:
		- tokenize([#"1",#"2",#"3"],"1a2b3c2b1");
		val it = ["a","b","c","b"] : string list
*)
fun 	tokenize( [] : char list, s : string) = [s]
	|tokenize( c : char list, s : string) = tokenize2(c,[s]);

(* returns a list containing n times the element given *)
fun 	fillList( el : int , 0 ) =  []
	|fillList( el , c ) = el::fillList(el,c-1);


(* returns a string representing an array of ints *)
fun 	printArray ( [] ) = ""
	|printArray( xs::xe : int list ) = ( Int.toString(xs)^" "^printArray(xe));
