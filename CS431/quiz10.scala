abstract class Expr
    case class Number(n: Int) extends Expr
    case class Sum(e1: Expr, e2: Expr) extends Expr
    case class Product(e1:Expr, e2:Expr) extends Expr
    def eval(e: Expr): Int = e match { 
            case Number(n) => n 
            case Sum(e1, e2) => eval(e1) + eval(e2)
            case Product(e1,e2) => eval(e1) * eval(e2)
}

//println(eval(Product(Number(5),Number(6))))