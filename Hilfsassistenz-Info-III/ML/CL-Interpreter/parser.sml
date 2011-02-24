
(* The parser function *)

(* Input:
	- a char list (the list of tokens to be parsed)
	- a collection (the partial result until now)
	- the Store (contains the collections on which the system works

   Output:
	- a collection with the partial result
*)
fun 	p([] : char list, r : Collection, s : Store) = r
	|p( ls::le , coll(n,b,c) : Collection , s : Store )=

		if (isChar #"+" ls) then 
			if (Char.compare(b,#"s")=EQUAL) then setUnion(coll(n,b,c),toSet(p(le,coll(#"_",#"b",[]),s)))
			else bagUnion(coll(n,b,c),toBag(p(le,coll(#"_",#"b",[]),s)))
		else if (isChar #"*" ls) then
			if (Char.compare(b,#"s")=EQUAL) then setIntersection(coll(n,b,c),toSet(p(le,coll(#"_",#"b",[]),s)))
			else bagIntersection(coll(n,b,c),toBag(p(le,coll(#"_",#"b",[]),s)))
		else if (isChar #"/" ls) then 
			if (Char.compare(b,#"s")=EQUAL) then setDifference(coll(n,b,c),toSet(p(le,coll(#"_",#"b",[]),s)))
			else bagDifference(coll(n,b,c),toBag(p(le,coll(#"_",#"b",[]),s)))
		else if Char.isAlpha(ls) then p(le,getColl(ls,s),s)
		else if (isChar #"(" ls) then p(le,coll(#"_",#"b",[]),s)
		else if (isChar #")" ls) then p(le,coll(n,b,c),s)
		else coll(n,b,c);

