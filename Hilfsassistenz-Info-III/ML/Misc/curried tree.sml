datatype 'a tree =
   empty
|  leaf of 'a
|  node of 'a * 'a tree * 'a tree;

fun expLeaves empty = empty
|   expLeaves (leaf(x)) = node(x, empty, empty)
|   expLeaves (node(x, l, r)) = node(x, expLeaves(l), expLeaves(r));

fun redTree f empty a = a
|   redTree f (leaf(x)) = f(x, a)
|   redTree f (node(x, l, r)) a =
             redTree f r (redTree f l (redTree f (leaf(x)) a));

val myTree = node(
   "top",
   node("left", empty, leaf("left-right")),
   node("right", leaf("right-left"), leaf("right-right")));