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
      |                TokIN               
      |                TokIDENT of string  
      |                TokEQUALS           
      |                TokNUMBER of int    
      |                TokHALT


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

      exception Nth
      fun nth([],n)           = raise Nth
      |   nth(first::rest, 0) = first
      |   nth(first::rest,n)  = nth(rest,n-1)


      (* function to find the length of a list *)

      fun length []       = 0
      |   length (hd::tl) = 1+ length tl
 

      (* functions to determine type of char in input *)

      fun BadLetter(char) = (char < "a" orelse char > "z")
                            andalso (char < "A" orelse char > "Z");

      fun IsASpace(str) = str <= " "

      fun IsAlphanum "" = false
      |   IsAlphanum ch = (ch >= "a" andalso ch <= "z")
                          orelse (ch >= "A" andalso ch <= "Z")
                          orelse (ch >= "0" andalso ch <= "9")

      fun Solo(sym) = exists (fn x => x = sym) ["(", ")", "+", "-", "*"]


      (* function to form "words" from "chars" in input by "glueing" chars *)

      fun Glue(accum, this :: rest) =
             if IsASpace(this) then
                (if accum = "" then Glue("", rest)
                               else accum :: Glue("", rest))
             else if (IsAlphanum accum <> IsAlphanum this) then
                (if accum = "" then Glue(this, rest)
                               else accum :: Glue(this, rest))
             else if Solo(this) orelse Solo(accum) then
                (if accum = "" then Glue(this, rest)
                               else accum :: Glue(this, rest))
             else Glue(accum^this, rest) 
      |   Glue(accum,nil) = if accum = "" then [] else [accum]
 

      (* functions to construct a number from digits *)   
 
      fun IsNumber(s) = not(exists (fn char => char < "0" orelse char > "9") (sexplode s))

      fun MakeNumber(digits) =
         let fun MakeNumber'(d :: drest, result) =
                      MakeNumber'(drest, result * 10 + ord(d) - ord(#"0")) 
             |   MakeNumber'(nil, result) = result
         in  MakeNumber'(explode digits, 0)
         end


      (* function to check whether a word is a legal identifier *)

      fun IsIdent(s) = not(exists BadLetter (sexplode s))
    
      
      (* function to generate tokens from words *)
                    
      exception Lexical of string

      fun MakeToken("(")       = TokOPENBR         
      |   MakeToken(")")       = TokCLOSEBR        
      |   MakeToken("+")       = TokOP(Union)      
      |   MakeToken("*")       = TokOP(Intersect)  
      |   MakeToken("-")       = TokOP(Minus)      
      |   MakeToken("val")     = TokDECL         
      |   MakeToken("insert")  = TokINSERT    
      |   MakeToken("delete")  = TokDELETE    
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

      fun Lex(input) = Glue("", input)

      fun lexical () =
          let val LexStrings = Lex (inputLineCs(stdIn))
          in 
            map MakeToken LexStrings
          end 


  (* -------------------------            Parser            ----------------------- *)


      exception SyntaxError of Token list

      fun syntaxError(x) = raise SyntaxError(x)

      fun ParseExpr(TokDECL::TokIDENT(ident)::TokEQUALS::TokBULK(bulk)::rest) =
              (DECLexpr (ident,bulk),rest)
                 
      |   ParseExpr(TokINSERT::TokNUMBER(number)::TokIN::TokIDENT(ident)::rest) =
               (INSERTexpr (number,ident),rest)

      |   ParseExpr(TokDELETE::TokNUMBER(number)::TokIN::TokIDENT(ident)::rest) =
               (DELETEexpr (number,ident),rest)
       
      |   ParseExpr(TokHALT::rest) =
               (HALTexpr,rest)

      |   ParseExpr(TokIDENT(ident)::rest) =
               ParseExprTail(IDENTexpr(ident),rest)

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



  (* ------------------------            Evaluator          ----------------------- *)

      
      fun create(ident,bulk) =
          ( case bulk of 
              Set => SetColl(ident,[])  
            | Bag => BagColl(ident,[])
            | Seq => SeqColl(ident,[])
          )

      fun evaluate (HALTexpr,S) = (true,S)
      |   evaluate (DECLexpr(ident,bulk),S)
                = (print("> ");print(ident);print(" = [ ]\n");  
                  (false,create(ident,bulk)::S) )
 
      |   evaluate ((_),S) = (print("> not yet implemented\n"); (false,S))


 
  (* ------------------------           Interpeter          ----------------------- *)

      fun interpreter(S) =
          ( case evaluate(parser(),S) of
               (true,S)   => print("\n> goodbye \n") 
            |  (false,S') => interpreter(S')
           )

      fun CL () = interpreter(nil)
