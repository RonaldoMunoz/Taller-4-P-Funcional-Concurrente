package taller4

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global
class NewtonParalela {

  // Las funciones derivar y evaluar son candidatas para la paralelización
  // ya que son operaciones independientes que pueden ser costosas en tiempo de ejecución.

  // Utilizamos Future para ejecutar estas funciones en paralelo.
  // Future devuelve un valor que puede no estar disponible todavía, ya que se está computando en otro hilo.

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

  // Para la función raizNewton, utilizamos for-comprehension para manejar los Futures.
  // Esto nos permite escribir código asincrónico de manera más legible y manejable.

  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Future[Double] = {
    val maxIterations = 1000 // Define el maximo de iteraciones
    (1 to maxIterations).foldLeft(Future.successful(x0)) { (futureX, _) =>
      futureX.flatMap { x =>
        if (ba(f, a, x)) Future.successful(x)
        else {
          val fx = evaluar(f, a, x)
          val dfx = evaluar(derivar(f, a), a, x)
          Future.successful(x - fx / dfx)
        }
      }
    }
  }
}