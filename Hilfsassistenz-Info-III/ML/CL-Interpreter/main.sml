(* Main file *)

(* Reads the other files *)
use("utils.sml");
use("collections.sml");
use("storage.sml");
use("parser.sml");

(* decides which operation has to be called *)
(*
	quit
	create
	insert
	expression with collections
*)
fun 	chooser ( xs : string list , c : Store) = 
		if String.compare(hd(xs),"quit")=EQUAL andalso List.length(xs)=1 then (false,c)
		else if String.compare(hd(xs),"create")=EQUAL andalso List.length(xs)=4 then (createColl xs c)
		else if String.compare(hd(xs),"insert")=EQUAL andalso List.length(xs)=4 then insert xs c
		else if String.compare(hd(xs),"delete")=EQUAL andalso List.length(xs)=4 then delete xs c
		else (* call the parser *)
			let
				val out=TextIO.print "\n\t\tResult "
				val r=printColl(p( (listToChar (fineTokenize(xs)) ),coll(#"_",#"b",[]),c))
			in
				(true,c)			
			end; 

(* 
	tokenize the input and 
	if it returns false, it means the CL interpreter has received a quit command
*)
fun 	prepareInput ( s : string, c : Store) =
		let
			val toks=tokenize([#" ",#"\n"],s);
		in
			if (List.length(toks) > 0 ) then chooser (toks,c)
			else (true,c)
		end;

(* Function that loops until it receives a false from the prepareInput function *)
fun interpreter(S) =
          ( case prepareInput(TextIO.inputLine TextIO.stdIn,S) of
               (false,S)   => 	let
					val t=TextIO.print "\n> goodbye \n"
			     	in
					(false,S)
				end
            |  (true,S) => 
				let
					val t=TextIO.print ("\t\tStorage content: "^printStorage(S)^"\n");
				in
					interpreter(S)
				end
           );

(* function used to start the interpreter *)
fun go () = interpreter(content[]);
