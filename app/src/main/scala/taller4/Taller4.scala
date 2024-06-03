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
    val expr6 = Expo(Atomo( 'x' ) ,Numero(3) )
    val expr = Suma(Expo(Atomo('x'), Numero(3)),Atomo('x'))
    val e3 = Suma( Resta ( Prod (Atomo( 'x') ,Atomo( 'x' ) ) , Numero ( 4.0 ) ) , Prod (Numero ( 3.0 ) ,Atomo( 'x' )))
    //println(objNewton.raizNewton(e3 , Atomo( 'x') , 2.0 , objNewton.buenaAprox))
    //val expr1 = Expo(Suma(Prod(Numero(2), Atomo('x')), Numero(3)), Numero(2)) // (2x + 3)^2
    val expr2 = Suma(Suma(Numero(0), Atomo('x')), Numero(5))
    //println(objNewton.mostrar(objNewton.limpiar(expr2)))
    println("Hola Mundo")
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
        println()
      }
    )
  }


}
