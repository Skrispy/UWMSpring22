(*Samuel Kersebet Homework 6*)
datatype grade = A | B | C | D | F;
(*Question 01*)
fun percent2grade x = if x>= 90.0 then A
                        else if x>=80.0 then B
                        else if x>= 70.0 then C
                        else if x>= 60.0 then D
                        else F;

fun grade2point A = 4.0
| grade2point B = 3.0
| grade2point C = 2.0
| grade2point D = 1.0
| grade2point F = 0.0;

fun gpa [] = 0.0
| gpa xs = foldl op+ 0.0 (map grade2point xs) / Real.fromInt(List.length(xs));

fun gpaFromPercent ys = gpa (map percent2grade ys); 

(*Question 02*)
datatype 'a tree =
Leaf |
Node of 'a tree * 'a * 'a tree;

fun max x y = if x > y then x else y;

fun height Leaf = 1
| height (Node(tl, a, tr)) = 1 + max(height tl) (height tr);

fun isBalanced Leaf = true
| isBalanced (Node(tl,a,tr)) = ((height tl) = (height tr)) andalso
                                isBalanced tl andalso isBalanced tr;


fun size Leaf = 1
| size (Node(tl, a, tr)) = 1 + (size tl) + (size tr);




fun makeBST [] f = Leaf
| makeBST (x::xs) f = 
    let
        fun insert Leaf a = Node(Leaf, a, Leaf)
        | insert (Node(l,x,r)) a = 
            if f(a, x) then Node((insert l a), x, r)
            else Node(l, x, (insert r a))
        val y = makeBST xs f
    in
      insert y x
    end;