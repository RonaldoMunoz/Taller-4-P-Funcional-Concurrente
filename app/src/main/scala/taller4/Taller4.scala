/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object Taller4 {

  def saludo() = "Taller 4"

  def main(args: Array[String]): Unit = {
    val objNewton = new Newton()
    val expr = Expo(Atomo('x'), Numero(3))
    println((objNewton.evaluar(expr,Atomo('x'),2.0)))
    /*    println("Hola Mundo")
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
        println()
      }
    )
  }
  */
  }
}
