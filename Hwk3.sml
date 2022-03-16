(*Homework 3 - Samuel Kersebet*)

(*Question 1*)

fun zip (nil nil) = nil
|(L1 nil) = nil
| zip (nil L2) = nil
| zip (a::ax) (b::bx) = (a,b)::(zip ax bx);

(*Question 2*)

fun unzip nil = (nil, nil)
| unzip (a,b)::rest = 
	let val (l1, l2) = unzip rest
	in (a::l1, b::l2) end;

(*Question 3*)

fun zip3 nil nil nil = nil
| zip3 (a, nil, nil) = nil
| zip3 (nil, b, nil) = nil
| zip3 (nil, nil, c) = nil
| zip3 (a::ax, b::bx, c::cx) = (a,b,c)::zip3(ax,bx,cx);

(*Question 4*)

fun unzip3 nil = ([], [], [])
| unzip3 (a,b,c)::rest = 
	let val (l1, l2, l3) = unzip3 rest
	in (a::l1, b::l2, c::l3) end;

(*Question 5*)

fun zipWithIndex ([]) = (null, null)
| zipWithIndex (a::rest) = 
  let
  val l = length rest
  val index = List.tabulate(l, fn x=>x)
  in (index,a)::zipWithIndex(rest) end;

(*Question 6*)
fun flatten [] = []
| flatten (a::rest) = a @ flatten rest;

(*Question 7*)
fun flatten2 [] = []
| flatten2 ((a,b)::rest) = a::b::flatten2 rest; 