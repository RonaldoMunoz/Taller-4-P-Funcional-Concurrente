package taller4

class Newton {

  def mostrar (e: Expr): String =
    e match {
      case Numero(d) => d.toString
      case Atomo(x) => x.toString
      case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
      case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
      case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
      case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
      case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
      case Logaritmo(e1) => s"(lg(${mostrar(e1)}))"
    }

  def main(): Unit = {
    import Expr._

    val expr1 = Suma(Atomo('x'), Numero(2))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val expr3 = Suma(expr1, Expo(expr2, Numero(5)))
    val expr4 = Logaritmo(Atomo('x'))
    val expr5 = Prod(Div(expr1, expr2), Resta(expr3, expr4))
    val expr6 = Expo(Atomo('x'), Numero(3))
  }
  println(mostrar(expr1))
  println(mostrar(expr2))
  println(mostrar(expr3))
  println(mostrar(expr4))
  println(mostrar(expr5))
  println(mostrar(expr6))
}
