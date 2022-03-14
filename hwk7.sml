
datatype exp = 
    Int of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of  exp * exp
  | Var of string
  | Let of string * exp * exp;

datatype value = 
    CVal of int
  | Error of string;

(* 
 * 1. implement 
 *      toString: exp -> string 
 *)
fun toString (Int x) = "x"
| toString (Plus e1 e2) = (toString e1) ^ " + " ^ (toString e2)
| toString (Minus e1 e2) = (toString e1) ^ " - " ^ (toString e2)
| toString (Times e1 e2) = (toString e1) ^ " * " ^ (toString e2)
|toString (Div e1 e2) = (toString e1) ^ " / " ^ (toString e2)
(* 2. implement 
 *       toStringValue: value -> string 
 *)
   
(* 
 * you may want to implement the helper function 
 *       lookup: (string * value) list -> string -> value 
 *)

(*
 * 3. implement the function 
 *       eval: exp -> (string * value) list -> value
 *
 * you may find 'case expression' useful in your implementation.
 * If you use case expressions in pattern matching, remember to wrap the case
 * expressions in paranthesis to avoid parsing errors.
 *)

(* Test Code *)

Control.Print.printDepth := 100;
Control.Print.stringDepth := 200;

val t1 = Let("y", Int 10, 
                Let("y", Int 20,
                    Times(Var "y", Int 5)));

toStringValue(eval t1 []);

val t2 = Let("y", Int 10, 
                  Let("z", Plus(Var "x", Var "y"), 
                             Times(Var "y", Int 5)));
toStringValue(eval t2 []);
                                   

val t3 = Div(Int 10, Int 0);
toStringValue(eval t3 []);

val t4 = Plus(Int 10, Minus (Int 20, Var("x")));
toStringValue(eval t4 []);

val t5 = Let("x", Plus(Int 10, t3), Plus(Int 0, Int 20));
toStringValue(eval t5 []);

