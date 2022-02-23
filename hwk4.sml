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
    let(*select helper function needs to build new list look at lecture recording*)
        fun select small ([], output) = small::(selection_sort output, f)
        | select small (x::rest, output) =
            if f(x,small) then
                select x (rest, small::output)
            else
                select small (rest,x::output)
    in
        select first (list, [])
    end;
