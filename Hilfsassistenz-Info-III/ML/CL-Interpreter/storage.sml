(* Definition of store *)

(* store is a list of Collection *)
datatype Store = content of Collection list;

(* returns the collection with the given name *)
fun	getColl(n: char , content([]) : Store) = coll(#"_",#"b",[])
	|getColl(n: char , content(coll(name,b,c)::cr) : Store ) =
		if Char.compare(n,name)=EQUAL then coll(name,b,c)
		else getColl(n, content(cr));

(* Checks if a collection with the given name exists in the store *)
fun 	existsColl(n : char, content([]) :  Store ) = false
	|existsColl(n : char, content(coll(name,_,_)::cr) : Store ) = 
		if Char.compare(n,name)=EQUAL then true
		else existsColl(n,content(cr));

(* Checks is a collection with the given name exists in the store *)
fun 	existsCollS(n : string, s : Store) = 
		existsColl( List.nth(String.explode(n),0),s);

(* Create a collection and insert it into the store *)
fun	createColl (x : string list) ( content(c) : Store) =
		if	String.compare( List.nth(x,0),"create")=EQUAL andalso
			(not (existsCollS(List.nth(x,1),content(c)))) andalso
			String.compare(List.nth(x,2),"as")=EQUAL andalso
			existsType(List.nth(x,3)) 
		then
			let
				val ttt=TextIO.print "Creating new collection\n";
			in
			(true,content(
				coll(
					List.nth(String.explode(List.nth(x,1)),0), 
					List.nth(String.explode(List.nth(x,3)),0),
					[]
				)::c))
			end
		else
			(true,content(c));			



(* insert the given element in the given collection *)
fun 	insertElementS3( [] : Collection list) (e : int) (n : char) = [] : Collection list
	|insertElementS3( coll(cn,cb,cc)::xs ) (e : int) (n : char) =
		if Char.compare(cn,n)=EQUAL then insertElement(coll(cn,cb,cc),e)::xs
		else coll(cn,cb,cc)::(insertElementS3 xs e n);

fun	insertElementS2 ( content(ls) : Store) ( e: int) (n: char) =
		content(insertElementS3 ls e n);

(* s - Store, e - Element to be inserted, n - name of the collection *)
fun 	insertElementS( s : Store) (e : string ) (n : string) =
		insertElementS2 s (stringToInt(e)) (stringToChar(n));
	
(* Insert a new value into a collection in the store *)
fun	insert ( x : string list) ( s : Store) =
		if	String.compare( List.nth(x,0),"insert")=EQUAL andalso
			existsCollS(List.nth(x,3),s) andalso
			String.compare(List.nth(x,2),"in")=EQUAL
		then
			let
				val name=List.nth(x,3)
				val element=List.nth(x,1)
			in
				(true, insertElementS s element name)
			end
		else
			(true,s);

(* delete the given element in the given collection *)
fun 	deleteElementS3( [] : Collection list) (e : int) (n : char) = [] : Collection list
	|deleteElementS3( coll(cn,cb,cc)::xs ) (e : int) (n : char) =
		if Char.compare(cn,n)=EQUAL then deleteElement(coll(cn,cb,cc),e)::xs
		else coll(cn,cb,cc)::(deleteElementS3 xs e n);

fun	deleteElementS2 ( content(ls) : Store) ( e: int) (n: char) =
		content(deleteElementS3 ls e n);

(* s - Store, e - Element to be deleted, n - name of the collection *)
fun 	deleteElementS( s : Store) (e : string ) (n : string) =
		deleteElementS2 s (stringToInt(e)) (stringToChar(n));
	
(* Delete a value from a collection in the store *)
fun	delete ( x : string list) ( s : Store) =
		if	String.compare( List.nth(x,0),"delete")=EQUAL andalso
			existsCollS(List.nth(x,3),s) andalso
			String.compare(List.nth(x,2),"from")=EQUAL
		then
			let
				val name=List.nth(x,3)
				val element=List.nth(x,1)
			in
				(true, deleteElementS s element name)
			end
		else
			(true,s);



(* print the list of the name of the collections in the storage *)
fun	printStorage ( content([]) : Store ) =
		"\n"
	|printStorage (content(coll(n,_,_)::xe) : Store) =
		" "^Char.toString(n)^" "^printStorage(content(xe)); 
