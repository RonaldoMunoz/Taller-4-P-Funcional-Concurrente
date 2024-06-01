package taller4


class EvalExpr {
  def Evaluar(e:Expr): Double = {
    e match {
      case Numero(d) => d
    }
  }
}
