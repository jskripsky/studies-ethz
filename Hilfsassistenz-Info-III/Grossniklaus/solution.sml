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
      |                     ASSIGNexpr of string * Expression
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


      fun IsASpace str = str <= " "

      
      fun noNumberChar "" = true
      |   noNumberChar ch = (ch < "0" orelse ch > "9")
      
                          
      fun noAlphaChar "" = false
      |   noAlphaChar ch = (ch < "a" orelse ch > "z")
                           andalso (ch < "A" orelse ch > "Z")

      fun noAlphanumChar "" = false
      |   noAlphanumChar ch = (noAlphaChar ch) andalso (noNumberChar ch)


      fun IsNumber s = not(exists noNumberChar (sexplode s))
      
      
      fun IsAlphanumber s = not(exists noAlphanumChar (sexplode s))
      

      fun Solo sym = exists (fn x => x = sym) ["(", ")", "+", "-", "*"]


      (* function to form "words" from "chars" in input by "glueing" chars *)

      fun Glue accum (this::rest) =
             if IsASpace this then
                (if accum = "" then Glue "" rest
                               else accum::(Glue "" rest))
             else if (IsAlphanumber accum <> IsAlphanumber this) then
                (if accum = "" then Glue this rest
                               else accum::(Glue this rest))
             else if Solo this orelse Solo accum then
                (if accum = "" then Glue this rest
                               else accum::(Glue this rest))
             else Glue (accum^this) rest
      |   Glue accum nil = if accum = "" then [] else [accum]
 

      (* functions to construct a number from digits *)   
 

      fun MakeNumber digits =
         let fun MakeNumber'(d::drest, result) =
                      MakeNumber'(drest, result * 10 + ord(d) - ord(#"0")) 
             |   MakeNumber'(nil, result) = result
         in  MakeNumber'(explode digits, 0)
         end


      (* function to check whether a word is a legal identifier *)

      
      fun checkIdentTail nil = true
      |   checkIdentTail (hd::tl) = (IsAlphanumber hd) andalso (checkIdentTail tl)
      
      
      fun checkIdent nil = true
      |   checkIdent (hd::tl) = not(noAlphaChar hd) andalso (checkIdentTail tl)
      
      fun IsIdent s = checkIdent (sexplode s)
    
      
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
              
      |   ParseExpr(TokDECL::TokIDENT(var)::TokEQUALS::rest) =
               let val (Expr',rest') = ParseExpr(rest)
               in (ASSIGNexpr(var, Expr'), rest')
               end
                                
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


  (* ------------------------       Support Functions       ----------------------- *)
  
 
      fun setContains elm nil = false
      |   setContains elm (hd::tl) = (hd = elm) orelse (setContains elm tl)
      
      
      fun bagContains elm nil = false
      |   bagContains elm ((num, cnt)::tl) = (num = elm) orelse bagContains elm tl
      
      
      fun bagCount elm nil = 0
      |   bagCount elm ((num, cnt)::tl) = 
            if num = elm then cnt
                         else bagCount elm tl
  

      fun seqContains seq nil = true
      |   seqContains nil arg = false
      |   seqContains (hdS::tlS) (hdA::tlA) = 
            if (hdS = hdA) then seqContains tlS tlA
                           else seqContains tlS (hdA::tlA)


      fun seqReplicate 0 seq = nil
      |   seqReplicate n seq = seq @ (seqReplicate (n-1) seq)

  
(* ------------------------       Insertion Functions      ------------------------ *)
  
      
      fun setInsert elm set = if (setContains elm set) then set
                                                       else elm::set;
                                                             

      fun bagInsert elm nil = [(elm, 1)]
      |   bagInsert elm ((num, cnt)::tl) = 
            if (elm = num) then (elm, cnt+1)::tl
                           else (num, cnt)::(bagInsert elm tl);
                           
                           
      fun seqInsert elm lst = lst @ [elm]
      
      
(* ------------------------        Deletion Functions      ------------------------ *)


      fun setDelete elm nil = nil
      |   setDelete elm (hd::tl) = 
            if hd = elm then tl
                        else hd::(setDelete elm tl)
                                                 
                                                 
      fun bagDelete elm nil = nil
      |   bagDelete elm ((num, cnt)::tl) =
            if num = elm then if cnt > 1 then (num, cnt-1)::tl
                                         else tl
                         else (num, cnt)::(bagDelete elm tl)
                           
                           
      fun bagRemove elm nil = nil
      |   bagRemove elm ((num, cnt)::tl) =
            if num = elm then tl
                         else (num, cnt)::(bagRemove elm tl)
                           
                           
      fun seqDelete elm nil = nil
      |   seqDelete elm (hd::nil) = if (hd = elm) then nil else (hd::nil)
      |   seqDelete elm (hd::tl) = hd::(seqDelete elm tl)
                            
                            
(* ------------------------      Collection Operations      ----------------------- *)

      
      fun setUnion set nil = set
      |   setUnion set (hd::tl) = 
            if setContains hd set then setUnion set tl
                                  else hd::(setUnion set tl)

      fun setIntersect set nil = nil
      |   setIntersect set (hd::tl) =
            if setContains hd set then hd::(setIntersect set tl)
                                  else setIntersect set tl
    

      fun setMinus nil set = nil
      |   setMinus (hd::tl) set = 
            if setContains hd set then setMinus tl set
                                  else hd::(setMinus tl set)
    

      fun bagUnion bag nil = bag
      |   bagUnion bag ((elm, cnt)::tl) = 
            (elm, Int.max(cnt, bagCount elm bag))::(bagUnion (bagRemove elm bag) tl)
            

      
      fun bagIntersect bag nil = nil
      |   bagIntersect bag ((elm, cnt)::tl) =
            if bagContains elm bag then 
               (elm, Int.min(cnt, bagCount elm bag))::(bagIntersect (bagRemove elm bag) tl)
            else
               bagIntersect (bagRemove elm bag) tl
               

      fun bagMinus nil bag = nil
      |   bagMinus ((elm, cnt)::tl) bag = 
            if bagContains elm bag then
               if cnt-(bagCount elm bag) > 0 then 
                  (elm, cnt-(bagCount elm bag))::(bagMinus tl bag)
               else
                  bagMinus tl bag
            else
               (elm, cnt)::bagMinus tl bag
      
      
      fun seqUnion seq arg = seq @ arg
      

      fun seqIntersect seq arg = 
         if (seqContains seq arg) then arg 
                                  else []
                                  
                                  
      fun seqMinus seq arg = 
         let 
            fun seqMinus' seq nil = seq
            |   seqMinus' nil arg = nil
            |   seqMinus' (hdS::tlS) (hdA::tlA) =
                  if (hdS = hdA) then seqMinus' tlS tlA
                                 else hdS::(seqMinus' tlS (hdA::tlA))
         in
            if (seqContains seq arg) then seqMinus' seq arg
                                     else seq
         end

  
(* ------------------------       Conversion Functions     ------------------------ *)
                           
           
      fun setToBag nil = nil
      |   setToBag (hd::tl) = (hd,1)::(setToBag tl)


      fun bagToSeq nil = nil
      |   bagToSeq ((elm, cnt)::tl) =
            (seqReplicate cnt [elm]) @ (bagToSeq tl)
      
      
      fun convert(SetColl(id1, lst1), BagColl(id2, lst2)) = 
            (BagColl(id1, (setToBag lst1)), BagColl(id2, lst2))
      |   convert(SetColl(id1, lst1), SeqColl(id2, lst2)) =
            (SeqColl(id1, lst1), SeqColl(id2, lst2))
      |   convert(BagColl(id1, lst1), SetColl(id2, lst2)) = 
            (BagColl(id1, lst1), BagColl(id2, (setToBag lst2)))
      |   convert(BagColl(id1, lst1), SeqColl(id2, lst2)) = 
            (SeqColl(id1, (bagToSeq lst1)), SeqColl(id2, lst2))
      |   convert(SeqColl(id1, lst1), SetColl(id2, lst2)) =
            (SeqColl(id1, lst1), SeqColl(id2, lst2))
      |   convert(SeqColl(id1, lst1), BagColl(id2, lst2)) =
            (SeqColl(id1, lst1), SeqColl(id2, (bagToSeq lst2)))
      |   convert(bulk1, bulk2) = (bulk1, bulk2)
           

(* ------------------------           Operations           ------------------------ *)


      fun union (SetColl(id1, lst1), SetColl(id2, lst2)) =
            SetColl("it", setUnion lst1 lst2)
      |   union (BagColl(id1, lst1), BagColl(id2, lst2)) =
            BagColl("it", bagUnion lst1 lst2)
      |   union (SeqColl(id1, lst1), SeqColl(id2, lst2)) =
            SeqColl("it", seqUnion lst1 lst2)
      |   union (_, _) = raise NotImplemented

      fun intersect (SetColl(id1, lst1), SetColl(id2, lst2)) = 
            SetColl("it", setIntersect lst1 lst2)
      |   intersect (BagColl(id1, lst1), BagColl(id2, lst2)) = 
            BagColl("it", bagIntersect lst1 lst2)
      |   intersect (SeqColl(id1, lst1), SeqColl(id2, lst2)) =
            SeqColl("it", seqIntersect lst1 lst2)
      |   intersect (_, _) = raise NotImplemented

      fun minus (SetColl(id1, lst1), SetColl(id2, lst2)) = 
            SetColl("it", setMinus lst1 lst2)
      |   minus (BagColl(id1, lst1), BagColl(id2, lst2)) = 
            BagColl("it", bagMinus lst1 lst2)
      |   minus (SeqColl(id1, lst1), SeqColl(id2, lst2)) =
            SeqColl("it", seqMinus lst1 lst2)
      |   minus (_, _) = raise NotImplemented

  
  (* ------------------------             Storage           ----------------------- *)
  
  
      fun create ident bulk S =
        ( case bulk of 
            Set => SetColl(ident,[])::S  
          | Bag => BagColl(ident,[])::S
          | Seq => SeqColl(ident,[])::S
        )
 
 
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


      fun clear ident nil = nil
      |   clear ident (SetColl(id, lst)::tl) =
            if (id = ident) then tl else SetColl(id, lst)::(clear ident tl)
      |   clear ident (BagColl(id, lst)::tl) =
            if (id = ident) then tl else BagColl(id, lst)::(clear ident tl)
      |   clear ident (SeqColl(id, lst)::tl) =
            if (id = ident) then tl else SeqColl(id, lst)::(clear ident tl)
          
          
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
      
      fun rename name (SetColl(_, lst)) = SetColl(name, lst)
      |   rename name (BagColl(_, lst)) = BagColl(name, lst)
      |   rename name (SeqColl(_, lst)) = SeqColl(name, lst)
      
      

  (* ------------------------             Output            ----------------------- *)

 
      fun printSet nil = ()
      |   printSet (hd::tl) = ( print(Int.toString(hd:int)); print(" "); printSet tl )


      fun printBag nil = ()
      |   printBag ((elm, cnt)::tl) = 
            ( print("("); print(Int.toString(elm)); print(","); 
              print(Int.toString(cnt));  print(") "); printBag tl )
              
      
      fun printBulk (SetColl(ident, lst)) = 
            ( print("> "); print(ident); print(" = [ "); printSet(lst); 
              print("] : set\n"); SetColl(ident, lst)
            )
      |   printBulk (BagColl(ident, lst)) =
            ( print("> "); print(ident); print(" = [ "); printBag(lst); 
              print("] : bag\n"); BagColl(ident, lst) 
            )
      |   printBulk (SeqColl(ident, lst)) = 
            ( print("> "); print(ident); print(" = [ "); printSet(lst); 
              print("] : seq\n"); SeqColl(ident, lst)
            )
      
      
      fun printIdent ident lst = ( printBulk(lookup ident lst); lst)
      
      
      fun printType bulk = 
        ( case bulk of
            Set => print("set")
          | Bag => print("bag")
          | Seq => print("seq")
        )
        
             
      fun printParseList nil = print("")
      |   printParseList (hd::tl) = 
          (
            ( case hd of
                 TokOPENBR    => print("( ")
               | TokCLOSEBR   => print(") ")
               | TokBULK(Set) => print("set ")
               | TokBULK(Bag) => print("bag ")
               | TokBULK(Seq) => print("seq ")
               | TokOP(Minus) => print("- ")
               | TokOP(Intersect) => print("* ")
               | TokOP(Union) => print("+ ")
               | TokDECL      => print("val ")
               | TokINSERT    => print("insert ")
               | TokDELETE    => print("delete ")
               | TokCLEAR     => print("clear ")
               | TokIN        => print("in ")
               | TokIDENT(s)  => ( print(s:string); print(" ") )
               | TokEQUALS    => print("= ")
               | TokNUMBER(i) => ( print(Int.toString(i)); print(" ") )
               | TokHALT      => print("halt")
             );
             printParseList tl 
          )
          
          
          fun printSyntaxError nil = print("> ERROR: incomplete statement\n")
          |   printSyntaxError (hd::tl) =
              ( print("> ERROR: syntax error (");
                ( case hd of
                     TokOPENBR => print("\"(\" not expected")
                   | TokCLOSEBR => print("\")\" not expected")
                   | TokBULK(Set) => print("illegal statement \"set\"")
                   | TokBULK(Bag) => print("illegal statement \"bag\"")
                   | TokBULK(Seq) => print("illegal statement \"seq\"")
                   | TokOP(Minus) => print("illegal operator \"-\"")
                   | TokOP(Intersect) => print("illegal operator \"*\"")
                   | TokOP(Union) => print("illegal operator \"+\"")
                   | TokDECL => print("illegal declare statement")
                   | TokINSERT => print("illegal insert statement")
                   | TokDELETE => print("illegal delete statement")
                   | TokCLEAR => print("illegal clear statement")
                   | TokIN => print("illegal statement \"in\"")
                   | TokIDENT(s) => 
                      ( print("illegal identifier \""); print(s:string); print("\""))
                   | TokEQUALS => print("illegal operator \"=\"")
                   | TokNUMBER(i) => 
                      ( print("illegal number \""); print(Int.toString(i)); 
                        print("\"")
                      )
                   | TokHALT => print("illegal halt statement")
                 );
                 print(")\n>        in statement: ");
                 printParseList (hd::tl);
                 print("\n")
               )
            


  (* ------------------------            Evaluator          ----------------------- *)

      
      fun evalOp (IDENTexpr(ident)) S = 
            lookup ident S
      |   evalOp (OPexpr(Union, exp1, exp2)) S = 
            union (convert((evalOp exp1 S), (evalOp exp2 S)))
      |   evalOp (OPexpr(Intersect, exp1, exp2)) S = 
            intersect (convert((evalOp exp1 S), (evalOp exp2 S)))
      |   evalOp (OPexpr(Minus, exp1, exp2)) S = 
            minus (convert((evalOp exp1 S), (evalOp exp2 S)))
      |   evalOp (_) (_) = raise NotImplemented


      fun evaluate (HALTexpr) S = (true,S)
      |   evaluate (DECLexpr(ident,bulk)) S = 
            if check ident S then
               ( print("> "); print(ident); print(" = [ ] : "); printType(bulk);
                 print("\n"); (false, (create ident bulk S)) 
               )
            else raise IllegalIdent(ident)
      |   evaluate (ASSIGNexpr(var, expr)) S =
            if check var S then
               ( false, printBulk(rename var (evalOp expr S))::(clear var S)
               )
            else raise IllegalIdent(var)
      |   evaluate (INSERTexpr(num, ident)) S = 
            (false, printIdent ident (insert num ident S))
      |   evaluate (DELETEexpr(num, ident)) S =
            (false, printIdent ident (delete num ident S))
      |   evaluate (CLEARexpr(ident)) S = 
            ( print("> clearing "); print(ident); print("\n"); 
              (false, (clear ident S))
            )
      |   evaluate (IDENTexpr(ident)) S = 
            (false, printIdent ident S)
      |   evaluate (OPexpr(opr, ex1, ex2)) S = 
            (false, printBulk(evalOp (OPexpr(opr, ex1, ex2)) S)::(clear "it" S))
      |   evaluate (_) S = raise NotImplemented


 
  (* ------------------------           Interpeter          ----------------------- *)

      fun interpreter(S) =
          ( case evaluate (parser()) S
               handle
                  NotImplemented => 
                     ( print("> ERROR: sorry, not implemented\n"); (false, S) )
               |  Overflow => 
                     ( print("> ERROR: number too big\n"); (false, S) )
               |  Nth =>
                     ( print("> ERROR: internal error nth\n"); (false, S) )
               |  Lexical(s) =>
                     ( print("> ERROR: lexical error ("); print(s); print(")\n"); 
                       (false, S) 
                     )
               |  SyntaxError(lst) =>
                     ( printSyntaxError lst; (false, S) )
               |  NoSuchIdent(s) =>
                     ( print("> ERROR: ident "); print(s); print(" unknown\n"); 
                       (false, S)
                     )
               |  IllegalIdent(s) =>
                     ( print("> ERROR: illegal ident ("); print(s); print(")\n"); 
                       (false, S) 
                     )      
            of
               (true,S)   => print("> goodbye\n\n") 
            |  (false,S') => interpreter(S')
           )

      fun CL () = 
         ( print("> cl system ready\n");
           interpreter(nil)
         )
      

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
   