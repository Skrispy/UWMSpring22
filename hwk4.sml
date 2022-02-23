(*Homework 4 - Samuel Kersebet*)

(*Question 1*)
fun is_sorted (nil, _) = true
| is_sorted ([x], f) = true
| is_sorted (x::y::list, f) = 
    if f(x,y) 
    then is_sorted(list, f)
    else false;

(*Question 2*)
fun selection_sort (nil, _) = []
    | selection_sort (list, f) =
        let
            fun select [] = (nil,nil)
            |select a = (a,[])
            |select (a::rest) =
                let 
                    val (m, rest') = select rest;
                in 
                    if f(m,a)
                    then (m,a::rest')
                    else (a, rest)
                end

        val (min ,list') = select list
        in 
            min::selection_sort(list,f)
        end;





(*Question 3*)
fun insertion_sort (nil, _) = []
| insertion_sort (list, f) = 
        let
            fun insert ([],x) = [x]
            | insert (l'::l,x) =
                if f(x, l') 
                then l'::insert(x,l)
                else x::l'::l

            val ret = []
            fun sort (_,nil) = []
            |sort (sofar,a::list) =
                insert(sofar,a)

            
        in 
            sort(ret,list)
        end;

