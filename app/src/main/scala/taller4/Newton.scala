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

  // 1.3 Derivar expresiones
  def derivar (f:Expr,a:Atomo): Expr = {
    f match {
      case Numero(d) => Numero(0)
      case Atomo(x) => if (x == a.x) Numero(1) else Numero(0)
      case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
      case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
      case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
      case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Expo(e2, Numero(2)))
      case Expo(e1, e2) => Prod(Expo(e1, e2), Prod(Suma(Div(Prod(derivar(e1, a), e2), e1), derivar(e2, a)), Logaritmo(e1)))
      case Logaritmo(e1) => Div(derivar(e1, a), e1)

    }
  }
  def limpiar(expr: Expr): Expr =
    expr match {
    case Suma(Numero(0), e) => limpiar(e)
    case Suma(e, Numero(0)) => limpiar(e)
    case Prod(Numero(1), e) => limpiar(e)
    case Prod(e, Numero(1)) => limpiar(e)
    case Prod(Numero(0), _) => Numero(0)
    case Prod(_, Numero(0)) => Numero(0)
    case Resta(e, Numero(0)) => limpiar(e)
    case Resta(e1, e2) if e1 == e2 => Numero(0)
    case Div(e, Numero(1)) => limpiar(e)
    case Expo(e, Numero(1)) => limpiar(e)
    case Expo(_, Numero(0)) => Numero(1)
    case Logaritmo(Numero(1)) => Numero(0)
    case Suma(e1, e2) => Suma(limpiar(e1), limpiar(e2))
    case Prod(e1, e2) => Prod(limpiar(e1), limpiar(e2))
    case Resta(e1, e2) => Resta(limpiar(e1), limpiar(e2))
    case Div(e1, e2) => Div(limpiar(e1), limpiar(e2))
    case Expo(e1, e2) => Expo(limpiar(e1), limpiar(e2))
    case Logaritmo(e) => Logaritmo(limpiar(e))
    case e => e
  }

  //1.4 Evaluar expresiones
  def evaluar (f:Expr , a:Atomo ,v:Double ):Double = {
    f match {
      case Numero(d) => d
      case Atomo(x) => if (x == a.x) v else 0
      case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
      case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
      case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
      case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
      case Expo(e1, e2) => Math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
      case Logaritmo(e1) => Math.log(evaluar(e1, a, v))
    }
  }

  // Raices Newton
  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    val maxIterations = 1000 // Define el maximo de iteraciones
    (1 to maxIterations).foldLeft(x0) { (x, _) =>
      if (ba(f, a, x)) x
      else {
        val fx = evaluar(f, a, x)
        val dfx = evaluar(derivar(f, a), a, x)
        x - fx / dfx
      }
    }
  }
  def buenaAprox( f : Expr , a : Atomo , d : Double): Boolean = {
    evaluar(f,a,d) < 0.001
  }
}
