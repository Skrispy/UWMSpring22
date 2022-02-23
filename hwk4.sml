(*Homework 4 - Samuel Kersebet*)

(*Question 1*)
fun is_sorted (nil, _) = true
| is_sorted ([x], f) = true
| is_sorted (x::y::list, f) = 
    if f(x,y) 
    then is_sorted(list, f)
    else false;

(*Question 2*)
fun selection_sort (nil, _) = nil
| selection_sort (list, f) =
    fun findMin [] = []
    | findMin [a] = (a,[])
    | findMin (a::rest) =
    let 
        val (a',rest') = findMin rest;
    in 
        if f(a',a)
        then a' , a::rest'
        else a, rest
    end;
let
    val (m,list') = findMin list;
in 
    m::selection_sort(list,f)
end;





(*Question 3*)
fun insertion_sort (nil, _) = nil
|