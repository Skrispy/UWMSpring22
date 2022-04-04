(*Hwk2 - Sam Kersebet *)

(*Question 1*)
fun gcd (a, 0) = a
| gcd (a, b) = gcd(b, (a mod b));

(*Question 2*)
fun simplify (a, b) =
	((a div gcd( a , b)), (b div gcd (a , b)));

(*Question 3*)
fun add ((a,b),(x,y)) = simplify (((a*y) + (x*b)),(b*y));

(*Question 4*)
fun times((a,b),(x,y)) =
	simplify((a*x),(b*y));

(*Question 5*)
fun addAll nil = (0,1)
| addAll ((a,b)::rest) = add((a,b),addAll(rest));

(*Question 6*)
fun timesAll nil = (1,1)
| timesAll ((a,b)::rest) = times((a,b),timesAll(rest));

(*Question 7*)
fun lessThan  ((a,b), (x,y)) = 
	if (a*y) < (x*b) then true else false;

(*Question 8*)
fun insert ((a,b), []) = [(a,b)]
| insert ((a,b), ((x,y)::rest)) = 
	if lessThan((a,b), (x,y))
	then (a,b)::(x,y)::rest
	else (x,y)::(insert((a,b), rest));

(*Question 9*)
fun sort [] = []
| sort ((a,b)::rest) = 
	insert ((a,b), (sort (rest));
