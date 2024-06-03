/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTaller4 extends AnyFunSuite {

    val newton = new Newton()
    val x = Atomo('x')
    val one = Numero(1)
    val two = Numero(2)
    val three = Numero(3)
    val five = Numero(5)
    val expr1 = Expo(Suma(Prod(two, x), three), two) // (2x + 3)^2
    val expr2 = Resta(Expo(x,x),Numero(100))

    test("Prueba 1 Mostrar") {
        assert(newton.mostrar(expr1) == "(((2.0 * x) + 3.0) ^ 2.0)")
    }
    test("Prueba 2 Mostrar") {
        assert(newton.mostrar(Suma(Prod(two, x), three)) == "((2.0 * x) + 3.0)")
    }
    test("Prueba 3 Mostrar"){
        assert(newton.mostrar(Prod(two, x)) == "(2.0 * x)")
    }
    test("Prueba 4 Mostrar"){
        val expre1=Suma(Atomo( 'x' ) , Numero(2) )
        val expre2=Prod (Atomo( 'x' ) , Atomo( 'x' ) )
        val expr3= Suma( expre1 , Expo( expre2 , Numero(5) ) )
        val expr4= Logaritmo (Atomo( 'x' ) )
        val expr5 = Prod ( Div ( expre1 , expre2 ) , Resta ( expr3 , expr4 ) )
        assert(newton.mostrar(expr5) == "(((x + 2.0) / (x * x)) * (((x + 2.0) + ((x * x) ^ 5.0)) - (lg(x))))")
    }

    test("Prueba 5 Mostrar"){
        assert(newton.mostrar(expr2) == "((x ^ x) - 100.0)")
    }
    test("Prueba 1 Derivar"){
        val expr6 = Expo(Atomo( 'x' ) ,Numero(3) )
        assert(newton.mostrar(newton.derivar(expr6,x)) == "((x ^ 3.0) * ((((1.0 * 3.0) / x) + 0.0) * (lg(x))))")

    }
    test("Prueba 2 Derivar"){
    val expr10 = Suma(Expo(Atomo('x'), Numero(3)),Atomo('x'))
        assert(newton.mostrar(newton.derivar(expr10,Atomo('x'))) == "(((x ^ 3.0) * ((((1.0 * 3.0) / x) + 0.0) * (lg(x)))) + 1.0)")
    }
    test("Prueba 3 Derivar"){
        val expr7 = Suma( Resta ( Prod (Atomo( 'x') ,Atomo( 'x' ) ) , Numero ( 4.0 ) ) , Prod (Numero ( 3.0 ) ,Atomo( 'x' )))
        assert(newton.mostrar(newton.derivar(expr7,Atomo('x'))) == "((((1.0 * x) + (x * 1.0)) - 0.0) + ((0.0 * x) + (3.0 * 1.0)))")
    }

    test("Prueba 4 Derivar") {
        val expr = Expo(Atomo('x'), Numero(2)) // x^2
        assert(newton.mostrar(newton.derivar(expr, Atomo('x'))) == "((x ^ 2.0) * ((((1.0 * 2.0) / x) + 0.0) * (lg(x))))") // 2x
    }
    test("Prueba 5 Derivar") {
        val expr = Prod(Atomo('x'), Atomo('x')) // x * x
        assert(newton.mostrar(newton.derivar(expr, Atomo('x'))) == "((1.0 * x) + (x * 1.0))") // 2x
    }
    test("Prueba 1 Limpiar") {
        val expr = Suma(Prod(Numero(0), Atomo('x')), Numero(5)) // 0x + 5
        assert(newton.mostrar(newton.limpiar(expr)) == "5.0") // 5
    }

    test("Prueba 2 Limpiar") {
        val expr = Prod(Suma(Atomo('x'), Numero(0)), Numero(1)) // (x + 0) * 1
        assert(newton.mostrar(newton.limpiar(expr)) == "x") // x
    }

    test("Prueba 3 Limpiar") {
        val expr = Expo(Atomo('x'), Numero(1)) // x^1
        assert(newton.mostrar(newton.limpiar(expr)) == "x") // x
    }

    test("Prueba 4 Limpiar") {
        val expr = Div(Atomo('x'), Numero(1)) // x / 1
        assert(newton.mostrar(newton.limpiar(expr)) == "x") // x
    }

    test("Prueba 5 Limpiar") {
        val expr = Resta(Atomo('x'), Numero(0)) // x - 0
        assert(newton.mostrar(newton.limpiar(expr)) == "x") // x
    }

    test("Prueba 1 raizNewton") {
        val expr = Resta (Prod(Atomo('x'), Atomo('x')), Numuero (2.0))
        assert(Math.abs(newton.raizNewton(expr, Atomo('x'), 1.0, newton.buenaAprox) - 2.0) < 0.001)
    }

    test("Prueba 2 raizNewton") {
        val expr = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
        assert(Math.abs(newton.raizNewton(expr, Atomo('x'), 1.0, newton.buenaAprox) - 2.0) < 0.001)
    }

    test("Prueba 3 raizNewton") {
        val expr = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0)), Prod(Numero(3.0), Atomo('x')))
        assert(Math.abs(newton.raizNewton(expr, Atomo('x'), 1.0, newton.buenaAprox)) < 0.001)
    }


}