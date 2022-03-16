
(* Name :   Samuel Kersebet                  *)

(* Section 1 -- Grammar


 Q1
<Int> ::= [0-9] | ~[0-9]


 Q2
<List> ::= "[" "]" | "[" <Group> "]"
<Group> ::= <Int> | <Int> "," <Group>


 Q3
<Sus> ::= "nil" | <Int> "::" <Sus>


 Q4
<Tree> ::= "Node(" <Tree> "," <Tree> ")" | <leaf>
<Leaf> ::= "L" <Int>


 Q5
<Tree> ::= "Node(" <Tree> "," <Int> "," <Tree> ")" | <Leaf>
<Leaf> ::= "L"

 
*)



(* Section 2 -- Short answer



 Q1
The key differences between functional and imparative programming
are that functional programming avoids changing state by mutating data
while imparative is constantly changing the state in a program.
Functional program avoids using loops and if/else statements by 
using recursion and higher order function calls. Imperative programming
does not have this elegance and uses the more bruteforce approach of 
looping, classes, objects etc. Functional programming may seem more complex
because less of the functionality is spelled out right in front of the developer.


 Q2
 This is a third order function
int -> int -> int -> int


 Q3
This is a second order function
int -> int list -> int

*)



(* Section 3 -- Pattern matching *)

fun min a b = if a > b then b else a;
fun max a b = if a > b then a else b;
 (* Q1 *)

fun median x y z = case(x<y,y<z,z<x) of
                (true,true,false) => y
                |(false,true,false) => x
                |(false,true,true) =>z
                |(true,false,true) =>x
                |(true, false, false) =>z
                |(false,false,true) =>y;

 (* Q2 *)
fun maxList nil = 0
| maxList(x::nil) = x
| maxList (x::xs) = let
                        val m = maxList(xs)
                    in 
                        max x m
                    end;

 (* Q3 *)
fun take n [] = []
| take 0 L = nil
|take n (x::xs) =  x::(take (n-1) xs);

 (* Q4 *)
fun drop n [] = []
| drop 0 L = L
| drop n (x::xs) = if n = 1 then xs else drop (n-1) xs;

 (* Q5 *)
 fun member(x,[]) = false
|   member(x,b::y) =
      if x=b then true
      else member(x,y);

fun intersection([],y) = []
|   intersection(a::x,y) =
      if member(a,y) then a::intersection(x,y)
      else intersection(x,y);

fun range _ _ [] = []
| range 0 n L = take n L
| range i 0 L = drop i L
| range i n L = 
                let
                    val d = take n L
                    val u = drop i L
                in
                    intersection (d, u)
                end;

 (* Q6 *)
fun get _ [] = []
| get i (x::xs) = if i = 0 then [x] else get (i-1) xs;
                

 (* Q7 *)
fun split n [] = ([],[])
| split n L = (take n L, drop n L);

 (* Q8 *)
fun hop n [] = [[]]
|hop n L = (take n L)::hop n (drop n L);
                
        

(* Section 4 -- Higher order functions *)

 (* Q1 *)
fun ith ((x::xr),i) = if i=0 then x else ith (xr,(i-1));
fun column M i = map(fn M => ith(M,i-1))M;


 (* Q2 *)
fun sumRow M = map(fn i => foldl (op +) 0 i) M;

(* Section 5 -- Algebraic datatype *)
datatype tree = Node of tree * int * tree
| Leaf
 (* Q1 *)
fun maxTree (Node(Leaf, a, Leaf)) = a
|maxTree (Node(tl, a, tr)) = 
                            let
                                val maxLeft = maxTree(tl);
                                val maxRight = maxTree(tr);
                            in
                                case(a>maxLeft, a>maxRight) of
                                (true,true) => a
                                |(false,true) => maxLeft
                                |(true,false) => maxRight
                            end;


 (* Q2 *)
fun mapTree f Leaf = Leaf
| mapTree f (Node(tl, a, tr)) = Node(mapTree f tl, f a, mapTree f tr);