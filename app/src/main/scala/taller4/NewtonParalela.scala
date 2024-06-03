package taller4

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

class NewtonParalela {

val newton = new Newton()
  // Utilizamos Future para ejecutar estas funciones en paralelo.
  // Future devuelve un valor que puede no estar disponible todavía, ya que se está computando en otro hilo.


  // Para la función raizNewton, utilizamos for-comprehension para manejar los Futures.
  // Esto nos permite escribir código asincrónico de manera más legible y manejable.

  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Future[Double] = {
    val maxIterations = 1000 // Define el maximo de iteraciones
    (1 to maxIterations).foldLeft(Future.successful(x0)) { (futureX, _) =>
      futureX.flatMap { x =>
        if (ba(f, a, x)) Future.successful(x)
        else {
          val fx = newton.evaluar(f, a, x)
          val dfx = newton.evaluar(newton.derivar(f, a), a, x)
          Future.successful(x - fx / dfx)
        }
      }
    }
  }
}