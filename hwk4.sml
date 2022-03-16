(*Homework 4 - Samuel Kersebet*)

(*Question 1*)
fun is_sorted (nil, _) = true
| is_sorted ([x], f) = true
| is_sorted (x::y::list, f) = 
    if f(x,y) 
    then is_sorted(list, f)
    else false;

(*Question 2*)
fun selectionsort f =
  fn  [] => []
  |    (first::last) =>
    let
      fun select small ([], output) = small::(selectionsort f output)
      |   select small (x::xs, output) =
            if f(x, small) then
              select x (xs, small::output)
            else
              select small (xs, x::output)
    in
      select first (last, [])
    end;




(*Question 3*)
fun insertsort f =
fn [] => []
  |  (x::xs) =>
    let fun insert (x ,[]) = [x]
          | insert (x , y::ys) =
              if f(x,y) then x::y::ys
              else y::insert(x, ys)
    in 

  let
    fun sort([], sofar) = sofar
      | sort (l::ls, sofar) = sort(ls , insert(l, sofar))

  in sort(x::xs, [])


  end
    end;

