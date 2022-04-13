package hwk9

abstract class Exp {
  // return the string representation of this expression 
  override def toString: String = this match {
    case Num x => this.toString
    case Plus x y => "(" ^ x.toString ^ " + " ^ y.toString ^ ")"
    case Minius x y => "(" ^ x.toString ^ " - " ^ y.toString ^ ")"
    case Times x y => "(" ^ x.toString ^ " * " ^ y.toString ^ ")"
    case Div x y => "(" ^ x.toString ^ " / " ^ y.toString ^ ")"
    case Var x => x
    case Let x e1 e2 =>"let val " ^ x ^ " = " ^ e1.toString ^ "in" ^ e2.toString ^ " end"
    case App e1 e2 => "(" ^ e1.toString ^ " " ^ e2.toString ^ ")"
    case Fn x e1 => "(fn " ^ x ^ "=> " e1.toString ^ ")"
    // TODO
  }
  
  // lookup 'y' in 'ctx'
  def lookup (ctx: List[(String, Value)], y: String): Value = ctx match {
    case Nil => Error ("Variable not found")
    case (h,k)::ctx => if (h == y) k else lookup ctx y
    // TODO
  }

  def eval: Value = this eval List()
  // evaluate 'this' expression with the context 'ctx'
  def eval (ctx: List[(String, Value)]): Value = this match {
    case Int x => CVal x
    case (Plus e1 e2) ctx => e1.eval(ctx) + e2.eval(ctx)
    case (Minus e1 e2) ctx => e1.eval(ctx) - e2.eval(ctx)
    case (Times e1 e2) ctx => e1.eval(ctx) * e2.eval(ctx)
    case (Div e1 e2) ctx => {
     var x = e1.eval(ctx) 
     var y = e2.eval(ctx)
     if(y=0) Error("Division by zero error")
     else x/y
    }
    case (Var x) ctx => lookup(ctx x)
    case (Let x e1 e2) ctx => e2.eval(x, e1.eval(ctx)::ctx)
    case (Fn x e) _ => FVal(x e)
    case (App e1 e2) ctx => {
      if(e1.getClass != FnVal) Error("Application Error " ^ e1.toString ^ "is not a function.")
      else if(e2.getClass != FnVal) Error("Application Error " ^ e2.toString ^ "is not a function.")
      else {
        val (x, e) = e1.eval(ctx)
        val v = e2.eval(ctx)
        e.eval((x, v)::ctx)
     }
    // TODO
  }
}

case class Num(x: Int) extends Exp
case class Plus(e1: Exp, e2: Exp) extends Exp
case class Minus(e1: Exp, e2: Exp) extends Exp
case class Times(e1: Exp, e2: Exp) extends Exp
case class Div(e1: Exp, e2: Exp) extends Exp
case class Var(x: String) extends Exp
case class Let(x: String, e1: Exp, e2: Exp) extends Exp
case class App(e1: Exp, e2: Exp) extends Exp
case class Fn(x: String, e: Exp) extends Exp


abstract class Value {
  // return the string representation of this value
  override def toString = this match {
    case CVal x => this.toString
    case FnVal x e ctx => x ^ " => " ^ toString e 
    case Error m => this.toString
    // TODO
  }
}
case class CVal(x: Int) extends Value
case class FnVal(x: String, e: Exp, ctx: List[(String, Value)]) extends Value
case class Error(m: String) extends Value

object Hwk9 {
  def main(args: Array[String]) {
    val t1 = Let("y", Num(10), 
                  Let("f", Fn("x", Plus(Var("x"), Var("y"))), 
                           Let("y", Num(20),
                                    App(Var("f"), Num(5)))))
                                    
    val t2 = Let("y", Num(10), 
                  Let("f", Fn("x", Plus(Var("x"), Var("z"))), 
                           Let("y", Num(20),
                                    App(Var("f"), Num(5)))))
    
    val t3 = Div(Num(10), Num(0))
    
    val t4 = Plus(Num(10), Minus (Num(10), Fn("x", Num(5))))
    
    val t5 = App(Num(10), Num(10))
    
    val t6 = Let("f", Num(10), App(Var("f"), Num(20)))
    
    val t7 = Let("x", Plus(Num(10), Fn("x", Var("x"))), Plus(Num(0), Num(20)))
    
    println(t1.eval)
    println(t2.eval)
    println(t3.eval)
    println(t4.eval)
    println(t5.eval)
    println(t6.eval)
    println(t7.eval)
  }
}

