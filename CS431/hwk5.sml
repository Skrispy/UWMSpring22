(*Homework 5 - Samuel Kersebet*)

fun reduce f (a::b) = foldl f a b;

fun zip (nil, _) = nil
| zip (_, nil) = nil
| zip (a::list1, b::list2) = (a, b) :: zip (list1, list2);

(*Question 1*)
fun vectorAdd l1 l2 = map op+ (zip (l1, l2));


(*Question 2*)
fun svProduct x 1 = map (fn y => y*x) 1;

(*Question 3*)
fun vmProduct l4 m1 = reduce vectorAdd (map (svProduct) (zip(l4,m)));

(*Question 4*)
fun matrixProduct m2 k = map (fn x => vmProduct x k) m2;