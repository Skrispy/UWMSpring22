datatype exp =
    Const of int
    | Plus of exp * exp
    | Minus of exp * exp
    | Times of exp * exp
    | Div of exp * exp;

 fun eval (Const x) = x
    | eval (Plus(x, y)) = eval(x) + eval(y)
    | eval (Minus(x,y)) = eval(x) - eval(y)
    |eval (Times (x,y)) = eval (x) * eval (y)
    | eval (Div (x, y)) = eval (x) div eval (y);