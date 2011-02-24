(* prerequisites: *)
fun reduce f nil a = a
  | reduce f (hd::tl) a = f (hd, reduce f tl a);

datatype 'a tree = empty | node of 'a * 'a tree * 'a tree;
datatype item = X | L | R;

(* all possible tree traversal orderings (reversed): *)
val preorder =   rev [X, L, R];
val preorder' =  rev [X, R, L];

val inorder =    rev [L, X, R];
val inorder' =   rev [R, X, R];

val postorder =  rev [L, R, X];
val postorder' = rev [R, L, X];

(* the main part: *)
fun redTree order f empty a = a
|   redTree order f (node(x, l, r)) a =
        reduce (fn (X,b) => f(x, b)
                |  (L,b) => redTree order f l b
                |  (R,b) => redTree order f r b) order a;


(* the traversal demo: *)
fun treeToString order t a = redTree order (fn (x,a) => a^" "^x) t a;

val myTree = node(
   "top",
   node("left", empty, node("left-right", empty, empty)),
   node("right", node("right-left", empty, empty), node("right-right", empty, empty)));

treeToString preorder myTree "Tree:";
