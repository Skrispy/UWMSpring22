datatype exp = ...
2 | Equal of exp * exp
3 | If of exp * exp * exp;
4
5 datatype value = BVal of bool | ...
6
7 fun print (Equal(x,y)) = "(" ^print(x)^ " = " ^print(y)^ ")"
8 | print (If(x,y,z)) = "if " ^print(x)^ "
9 then " ^print(y)^ " else " ^print(z)
10 | ...
11
12 fun eval (Equal(e1, e2)) ctx =
13  let
14   val (CVal x) = eval e1 ctx
15   val (CVal y) = eval e2 ctx
16  in
17   BVal (round x = round y)
18  end
19 | eval (If(cond, e1, e2)) =
        case (eval(cond,ctx)) of
            val (BVal b) = if b then (eval(e1,ctx) else (eval(e2,ctx));
