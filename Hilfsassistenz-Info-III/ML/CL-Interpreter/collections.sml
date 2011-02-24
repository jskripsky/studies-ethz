(* Collection definition *)
(* Name, bulk (set,bag), element list *)
datatype Collection = coll of char * char * int list;

(* Manages only set (without duplicates) and bags (with duplicates) *)
(* returns true if the string represents a valid type *)
fun	existsType(s : string) =	
		if 	String.compare (s,"bag") = EQUAL orelse
			String.compare (s,"set") = EQUAL
		then 
			true
		else	
			false;

(* Converts a bag collection to a set collection *)
fun	toSet(coll(n,_,l)) = coll(n,#"s",setof l);
 
(* Converts a set collection to a bag collection *)
fun 	toBag(coll(n,_,l)) = coll(n,#"b",l);

(* Algebra operations *)

(* insert an element into a collection *)
fun	insertElement( coll(n1,b,c), n : int) =
		if Char.compare(b,#"s")=EQUAL then
			if (n mem c) then coll(n1,b,c)
			else coll(n1,b,n::c)
		else
			coll(n1,b,n::c);

(* delete an element from a collection *)
fun	deleteElement( coll(n1,b,c), n : int) = coll(n1,b,del(n,c));

(* helper functions *)
fun	reduce f nil a = a
|    	reduce f (hd::tl) a = f (hd, reduce f tl a)

fun	combine f l1 l2 = reduce
		(fn (x,a) => a@fillList(x, f(count(x,l1), count(x,l2))))
		(singleList(l1,l2))
                [];


(* bag operations *)
fun	bagUnion ( coll(n1,_,l1), coll(n2,_,l2)) =
		coll (#"_",#"b",combine (fn (c1,c2) => max(c1,c2)) l1 l2);
 
fun	bagIntersection( coll(n1,_,l1), coll(n2,_,l2)) =
		coll (#"_",#"b",combine (fn (c1,c2) => min(c1,c2)) l1 l2);

fun 	bagDifference ( coll(n1,_,l1), coll(n2,_,l2)) =
		coll (#"_",#"b",combine (fn (c1,c2) => if (c1-c2)>0 then c1-c2 else 0) l1 l2);

(* set operations *)
fun	setUnion ( coll(n1,_,l1), coll(n2,_,l2)) =
		coll (#"_",#"s",singleList(l1,l2));
 
fun	setIntersection( coll(n1,_,l1), coll(n2,_,l2)) =
		coll (#"_",#"s",combine (fn (1,1) => 1 | (_,_) => 0) l1 l2);

fun 	setDifference ( coll(n1,_,l1), coll(n2, _,l2)) =
		coll (#"_",#"s",combine (fn (1,0) => 1 | (_,_) => 0) l1 l2);


(* prints the given collection on screen *)
fun 	printColl ( coll(n,b,c) )=
		TextIO.print ("Collection: "^Char.toString(n)^" with bulk "^Char.toString(b)^" contains ["^printArray(c)^"]\n");
