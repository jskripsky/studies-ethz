 (* ------------------------       Interpreter for CL      ----------------------- *)

      
     open TextIO;


  (* ------------------------            Data Types         ----------------------- *)

      datatype Bulk = Set | Bag | Seq

      datatype Operator = Union | Intersect | Minus

      datatype Binding = SetColl of string * int list          
      |                  BagColl of string * (int * int) list  
      |                  SeqColl of string * int list
                
      type Store   = Binding list

      datatype Expression = DECLexpr of string * Bulk                     
      |                     INSERTexpr of int * string                    
      |                     DELETEexpr of int * string    
      |                     CLEARexpr of string
      |                     OPexpr of Operator * Expression * Expression  
      |                     IDENTexpr of string                           
      |                     NUMBERexpr of int                            
      |                     HALTexpr


      datatype Token = TokOPENBR          
      |                TokCLOSEBR         
      |                TokBULK of Bulk     
      |                TokOP of Operator   
      |                TokDECL             
      |                TokINSERT           
      |                TokDELETE 
      |                TokCLEAR
      |                TokIN               
      |                TokIDENT of string  
      |                TokEQUALS           
      |                TokNUMBER of int    
      |                TokHALT


  (* ------------------------           Exceptions          ----------------------- *)
  
  exception NotImplemented

  exception Nth

  exception Lexical of string

  exception SyntaxError of Token list
  
  exception NoSuchIdent of string
  
  exception IllegalIdent of string
  
  (* ------------------------        Lexical Analyser       ----------------------- *)
  

      (* function to input a line and return it as a list of chars as string values *)

      fun inputLineCs istream =
          let val char = inputN(istream,1)
          in
            if char = "\n" then []
            else char :: inputLineCs istream
          end


      (* function to check whether there exists element in list for which f is true *)

      fun exists f []            = false
      |   exists f (first::rest) = f first orelse exists f rest
      

      (* function to convert string to list of chars represented as strings *)

      fun sexplode s = map Char.toString (explode s);


      (* function to find nth element in a list *)

      fun nth nil n      = raise Nth
      |   nth (hd::tl) 0 = hd
      |   nth (hd::tl) n = nth tl (n-1)


      (* function to find the length of a list *)

      fun length []       = 0
      |   length (hd::tl) = 1 + length tl
 

      (* functions to determine type of char in input *)

      fun BadLetter char = (char < "a" orelse char > "z")
                           andalso (char < "A" orelse char > "Z");

      fun IsASpace str = str <= " "

      fun IsAlphanum "" = false
      |   IsAlphanum ch = (ch >= "a" andalso ch <= "z")
                          orelse (ch >= "A" andalso ch <= "Z")
                          orelse (ch >= "0" andalso ch <= "9")

      fun Solo sym = exists (fn x => x = sym) ["(", ")", "+", "-", "*"]


      (* function to form "words" from "chars" in input by "glueing" chars *)

      fun Glue accum (this::rest) =
             if IsASpace this then
                (if accum = "" then Glue "" rest
                               else accum::(Glue "" rest))
             else if (IsAlphanum accum <> IsAlphanum this) then
                (if accum = "" then Glue this rest
                               else accum::(Glue this rest))
             else if Solo this orelse Solo accum then
                (if accum = "" then Glue this rest
                               else accum::(Glue this rest))
             else Glue (accum^this) rest
      |   Glue accum nil = if accum = "" then [] else [accum]
 

      (* functions to construct a number from digits *)   
 
      fun IsNumber s = not(exists (fn char => char < "0" orelse char > "9") (sexplode s))

      fun MakeNumber digits =
         let fun MakeNumber'(d::drest, result) =
                      MakeNumber'(drest, result * 10 + ord(d) - ord(#"0")) 
             |   MakeNumber'(nil, result) = result
         in  MakeNumber'(explode digits, 0)
         end


      (* function to check whether a word is a legal identifier *)

      fun IsIdent(s) = not(exists BadLetter (sexplode s))
    
      
      (* function to generate tokens from words *)
                    
      fun MakeToken("(")       = TokOPENBR         
      |   MakeToken(")")       = TokCLOSEBR        
      |   MakeToken("+")       = TokOP(Union)      
      |   MakeToken("*")       = TokOP(Intersect)  
      |   MakeToken("-")       = TokOP(Minus)      
      |   MakeToken("val")     = TokDECL         
      |   MakeToken("insert")  = TokINSERT    
      |   MakeToken("delete")  = TokDELETE
      |   MakeToken("clear")   = TokCLEAR
      |   MakeToken("in")      = TokIN            
      |   MakeToken("=")       = TokEQUALS         
      |   MakeToken("halt")    = TokHALT        
      |   MakeToken("set")     = TokBULK(Set)    
      |   MakeToken("bag")     = TokBULK(Bag)    
      |   MakeToken("seq")     = TokBULK(Seq)    
      |   MakeToken(s)         = if IsNumber(s) then TokNUMBER(MakeNumber s)
                                 else if IsIdent(s) then TokIDENT(s)
                                 else raise Lexical(s)


      (* functions to perform lexical analysis of input, generating token list  *)
      (* the list of input characters are first "glued" into words and from the *)
      (* words, lexical tokens of the language are generated                    *)

      fun Lex(input) = Glue "" input

      fun lexical () =
          let val LexStrings = Lex (inputLineCs(stdIn))
          in 
            map MakeToken LexStrings
          end 


  (* -------------------------            Parser            ----------------------- *)


      fun ParseExpr(TokDECL::TokIDENT(ident)::TokEQUALS::TokBULK(bulk)::rest) =
              (DECLexpr(ident,bulk), rest)
                 
      |   ParseExpr(TokINSERT::TokNUMBER(number)::TokIN::TokIDENT(ident)::rest) =
               (INSERTexpr(number,ident), rest)

      |   ParseExpr(TokDELETE::TokNUMBER(number)::TokIN::TokIDENT(ident)::rest) =
               (DELETEexpr(number,ident), rest)
               
      |   ParseExpr(TokCLEAR::TokIDENT(ident)::rest) =
               (CLEARexpr(ident), rest)
       
      |   ParseExpr(TokHALT::rest) =
               (HALTexpr, rest)

      |   ParseExpr(TokIDENT(ident)::rest) =
               ParseExprTail(IDENTexpr(ident), rest)

      |   ParseExpr(TokOPENBR::rest) =
               ( case ParseExpr(rest) of
                   ( Expr,TokCLOSEBR::rest' ) => ParseExprTail(Expr,rest') 
                 | ( _,rest') => raise SyntaxError(rest')
               )

      |   ParseExpr(junk) = raise SyntaxError(junk) 

      and

          ParseExprTail(Expr,TokOP(Op)::rest) = 
               let val (Expr',rest') = ParseExpr(rest)
               in (OPexpr(Op,Expr,Expr'),rest')
               end

      |   ParseExprTail(Expr,rest) = (Expr,rest) 

     
      fun parser () =
         (  case ParseExpr(lexical()) of
                (tree,[])   => tree  
            |   (tree,rest) => raise SyntaxError(rest)
         )


  (* ------------------------     Collection Operations     ----------------------- *)
  
      fun isMember elm nil = false
      |   isMember elm (hd::tl) = (hd = elm) orelse (isMember elm tl);
  
  
  (* ------------------------   Implement these Functions   ----------------------- *)
  
      fun setToBag lst = []:((int*int) list);
      
      fun setUnion lst1 lst2 = lst1 @ lst2;
    
      fun setIntersect lst1 lst2 = lst1;
    
      fun setMinus lst1 lst2 = lst1;
    
      fun bagUnion lst1 lst2 = lst1 @ lst2;
      
      fun bagIntersect lst1 lst2 = lst1;
      
      fun bagMinus lst1 lst2 = lst1;

  (* ------------------------------------------------------------------------------ *)
  
  
      fun setInsert elm set = if (isMember elm set) then set
                                                    else elm::set;
                                                             

      fun bagInsert elm nil = [(elm, 1)]
      |   bagInsert elm ((num, cnt)::tl) = 
            if (elm = num) then (elm, cnt+1)::tl
                           else (num, cnt)::(bagInsert elm tl);
                           
                           
      fun seqInsert elm lst = lst @ [elm]
      
      
      fun setDelete elm nil = nil
      |   setDelete elm (hd::tl) = 
            if (hd = elm) then tl
                          else hd::(setDelete elm tl)
                                                 
                                                 
      fun bagDelete elm nil = nil
      |   bagDelete elm ((num, cnt)::tl) =
            if (num = elm) then if (cnt > 1) then (num, cnt-1)::tl
                                             else tl
                           else (num, cnt)::(bagDelete elm tl)
                           
                           
      fun seqDelete elm nil = nil
      |   seqDelete elm (hd::nil) = if (hd = elm) then nil else (hd::nil)
      |   seqDelete elm (hd::tl) = hd::(seqDelete elm tl)
                            
           
      fun convert(SetColl(id1, lst1), BagColl(id2, lst2)) = 
            ( BagColl(id1, setToBag(lst1)), BagColl(id2, lst2) )
      |   convert(BagColl(id1, lst1), SetColl(id2, lst2)) = 
            ( BagColl(id1, lst1), BagColl(id2, setToBag(lst2)) )
      |   convert(bulk1, bulk2) = (bulk1, bulk2)
           

      fun union(SetColl(id1, lst1), SetColl(id2, lst2)) =
            SetColl("it", setUnion lst1 lst2)
      |   union(BagColl(id1, lst1), BagColl(id2, lst2)) =
            BagColl("it", bagUnion lst1 lst2)
      |   union(_, _) = raise NotImplemented

      fun intersect(SetColl(id1, lst1), SetColl(id2, lst2)) = 
            SetColl("it", setIntersect lst1 lst2)
      |   intersect(BagColl(id1, lst1), BagColl(id2, lst2)) = 
            BagColl("it", bagIntersect lst1 lst2)
      |   intersect(_, _) = raise NotImplemented

      fun minus(SetColl(id1, lst1), SetColl(id2, lst2)) = 
            SetColl("it", setMinus lst1 lst2)
      |   minus(BagColl(id1, lst1), BagColl(id2, lst2)) = 
            BagColl("it", bagMinus lst1 lst2)
      |   minus(_, _) = raise NotImplemented

  
  (* ------------------------             Storage           ----------------------- *)
  
  
      fun insert elm ident nil = nil
      |   insert elm ident (SetColl(id, lst)::tl) = 
            if (id = ident) then SetColl(id, (setInsert elm lst))::tl
                            else SetColl(id, lst)::(insert elm ident tl)
      |   insert elm ident (BagColl(id, lst)::tl) = 
            if (id = ident) then BagColl(id, (bagInsert elm lst))::tl
                            else BagColl(id, lst)::(insert elm ident tl)
      |   insert elm ident (SeqColl(id, lst)::tl) = 
            if (id = ident) then SeqColl(id, (seqInsert elm lst))::tl
                            else SeqColl(id, lst)::(insert elm ident tl)
      
      
      fun delete elm ident nil = nil
      |   delete elm ident (SetColl(id, lst)::tl) =
            if (id = ident) then SetColl(id, (setDelete elm lst))::tl
                            else SetColl(id, lst)::(delete elm ident tl)
      |   delete elm ident (BagColl(id, lst)::tl) =
                  if (id = ident) then BagColl(id, (bagDelete elm lst))::tl
                                  else BagColl(id, lst)::(delete elm ident tl)
      |   delete elm ident (SeqColl(id, lst)::tl) = 
                  if (id = ident) then SeqColl(id, (seqDelete elm lst))::tl
                                  else SeqColl(id, lst)::(delete elm ident tl)
          
          
      fun lookup ident nil = raise NoSuchIdent(ident)
      |   lookup ident (SetColl(id, lst)::tl) = 
            if (ident = id) then SetColl(id, lst)
                            else lookup ident tl
      |   lookup ident (BagColl(id, lst)::tl) = 
            if (ident = id) then BagColl(id, lst)
                            else lookup ident tl
      |   lookup ident (SeqColl(id, lst)::tl) = 
            if (ident = id) then SeqColl(id, lst)
                            else lookup ident tl
                            
                            
      fun check ident nil = (ident <> "it")
      |   check ident (SetColl(id, _)::tl) = (ident <> id) andalso check ident tl
      |   check ident (BagColl(id, _)::tl) = (ident <> id) andalso check ident tl
      |   check ident (SeqColl(id, _)::tl) = (ident <> id) andalso check ident tl                            
      
      

  (* ------------------------             Output            ----------------------- *)

 
      fun printSet nil = ()
      |   printSet (hd::tl) = ( print(Int.toString(hd:int)); print(" "); printSet tl )


      fun printBag nil = ()
      |   printBag ((elm, cnt)::tl) = 
            ( print("("); print(Int.toString(elm)); print(","); 
              print(Int.toString(cnt));  print(") "); printBag tl )
              
      
      fun printBulk (SetColl(ident, lst)) = 
            ( print("> "); print(ident); print(" = [ "); printSet(lst); print("]\n") )
      |   printBulk (BagColl(ident, lst)) = 
            ( print("> "); print(ident); print(" = [ "); printBag(lst); print("]\n") )
      |   printBulk (SeqColl(ident, lst)) = 
            ( print("> "); print(ident); print(" = [ "); printSet(lst); print("]\n") )
      
      
      fun printIdent ident lst = ( printBulk(lookup ident lst); lst)
      


  (* ------------------------            Evaluator          ----------------------- *)

      
      fun create ident bulk =
          ( case bulk of 
              Set => SetColl(ident,[])  
            | Bag => BagColl(ident,[])
            | Seq => SeqColl(ident,[])
          )

      
      fun evaluateOp (IDENTexpr(ident)) S = 
            lookup ident S
      |   evaluateOp (OPexpr(Union, exp1, exp2)) S = 
            union(convert((evaluateOp exp1 S), (evaluateOp exp2 S)))
      |   evaluateOp (OPexpr(Intersect, exp1, exp2)) S = 
            intersect(convert((evaluateOp exp1 S), (evaluateOp exp2 S)))
      |   evaluateOp (OPexpr(Minus, exp1, exp2)) S = 
            minus(convert((evaluateOp exp1 S), (evaluateOp exp2 S)))
      |   evaluateOp (_) (_) = raise NotImplemented

      
      fun evaluate (HALTexpr) S = (true,S)
      |   evaluate (DECLexpr(ident,bulk)) S = 
            if check ident S then
               ( print("> "); print(ident); print(" = [ ]\n"); 
                 (false, (create ident bulk)::S) 
               )
            else raise IllegalIdent(ident)
      |   evaluate (INSERTexpr(num, ident)) S = 
            (false, printIdent ident (insert num ident S))
      |   evaluate (DELETEexpr(num, ident)) S =
            (false, printIdent ident (delete num ident S))
      |   evaluate (CLEARexpr(ident)) S = 
            ( print("> clearing "); print(ident); print("\n"); (false, S) )
      |   evaluate (IDENTexpr(ident)) S = 
            (false, printIdent ident S)
      |   evaluate (OPexpr(operation, exp1, exp2)) S = 
            ( printBulk(evaluateOp (OPexpr(operation, exp1, exp2)) S); (false, S) )
      |   evaluate (_) S = raise NotImplemented


 
  (* ------------------------           Interpeter          ----------------------- *)

      fun interpreter(S) =
          ( case evaluate (parser()) S
               handle
                  NotImplemented => 
                     ( print("> sorry, not implemented\n"); (false, S) )
               |  Overflow => 
                     ( print("> number too big\n"); (false, S) )
            of
               (true,S)   => print("\n> goodbye \n") 
            |  (false,S') => interpreter(S')
           )

      fun CL () = interpreter(nil)
      

  (* ------------------------             Usage             ----------------------- *)
  (*                                                                                *)
  (*   val "name" = [set|bag|seq]           -  creates a new binding                *)
  (*   insert "number" in "name"            -  inserts the number into the binding  *)
  (*   delete "number" in "name"            -  deletes the number in the binding    *)
  (*   clear "name"                         -  removes the binding                  *)
  (*   "name"                               -  displays the binding                 *)
  (*   expression                           -  evaluates the expression             *)
  (*   halt                                 -  stops the interpreter                *)
  (*                                                                                *)
  (* ------------------------------------------------------------------------------ *)
   