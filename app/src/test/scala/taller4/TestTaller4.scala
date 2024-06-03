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
    val complexExpr = Expo(Suma(Prod(two, x), three), two) // (2x + 3)^2

    test("mostrar debe mostrar correctamente las expresiones complejas") {
        assert(newton.mostrar(complexExpr) == "(((2.0 * x) + 3.0) ^ 2.0)")
    }

    test("mostrar debe manejar correctamente los números negativos") {
        val negativeExpr = Resta(two, three) // 2 - 3
        assert(newton.mostrar(negativeExpr) == "(2.0 - 3.0)")
    }

    test("mostrar debe manejar correctamente las divisiones") {
        val divExpr = Div(two, three) // 2 / 3
        assert(newton.mostrar(divExpr) == "(2.0 / 3.0)")
    }

    test("mostrar debe manejar correctamente las exponenciaciones") {
        val expoExpr = Expo(two, three) // 2 ^ 3
        assert(newton.mostrar(expoExpr) == "(2.0 ^ 3.0)")
    }

    test("mostrar debe manejar correctamente los logaritmos") {
        val logExpr = Logaritmo(two) // log(2)
        assert(newton.mostrar(logExpr) == "(lg(2.0))")
    }

    test("derivar debe derivar correctamente las expresiones complejas") {
        assert(newton.mostrar(newton.derivar(complexExpr, x)) == "((2.0 * ((2.0 * x) + 3.0)) * ((2.0 * 1.0) + 0.0))")
    }

    test("derivar debe manejar correctamente los números negativos") {
        val negativeExpr = Resta(two, three) // 2 - 3
        assert(newton.mostrar(newton.derivar(negativeExpr, x)) == "(0.0 - 0.0)")
    }

    test("derivar debe manejar correctamente las divisiones") {
        val divExpr = Div(two, three) // 2 / 3
        assert(newton.mostrar(newton.derivar(divExpr, x)) == "(((0.0 * 3.0) - (2.0 * 0.0)) / (3.0 ^ 2.0))")
    }

    test("derivar debe manejar correctamente las exponenciaciones") {
        val expoExpr = Expo(two, three) // 2 ^ 3
        assert(newton.mostrar(newton.derivar(expoExpr, x)) == "((2.0 ^ 3.0) * ((0.0 * 3.0 / 2.0) + 0.0) * (lg(2.0)))")
    }

    test("derivar debe manejar correctamente los logaritmos") {
        val logExpr = Logaritmo(two) // log(2)
        assert(newton.mostrar(newton.derivar(logExpr, x)) == "(0.0 / 2.0)")
    }

    test("evaluar debe evaluar correctamente las expresiones complejas") {
        assert(newton.evaluar(complexExpr, x, 1.0) == 25.0)
    }

    test("evaluar debe manejar correctamente los números negativos") {
        val negativeExpr = Resta(two, three) // 2 - 3
        assert(newton.evaluar(negativeExpr, x, 1.0) == -1.0)
    }

    test("evaluar debe manejar correctamente las divisiones") {
        val divExpr = Div(two, three) // 2 / 3
        assert(newton.evaluar(divExpr, x, 1.0) == 2.0 / 3.0)
    }

    test("evaluar debe manejar correctamente las exponenciaciones") {
        val expoExpr = Expo(two, three) // 2 ^ 3
        assert(newton.evaluar(expoExpr, x, 1.0) == 8.0)
    }

    test("evaluar debe manejar correctamente los logaritmos") {
        val logExpr = Logaritmo(two) // log(2)
        assert(newton.evaluar(logExpr, x, 1.0) == 0.6931471805599453)
    }

    test("raizNewton debe encontrar correctamente las raíces de las expresiones complejas") {
        val f = Resta(Expo(x, two), three) // x^2 - 3
        val root = newton.raizNewton(f, x, 1.0, newton.buenaAprox)
        assert(Math.abs(root - Math.sqrt(3)) < 0.001)
    }

    test("raizNewton debe manejar correctamente los números negativos") {
        val f = Resta(Expo(x, two), two) // x^2 - 2
        val root = newton.raizNewton(f, x, -1.0, newton.buenaAprox)
        assert(Math.abs(root + Math.sqrt(2)) < 0.001)
    }

    test("raizNewton debe manejar correctamente las divisiones") {
        val f = Resta(Div(x, two), one) // x/2 - 1
        val root = newton.raizNewton(f, x, 1.0, newton.buenaAprox)
        assert(Math.abs(root - 2.0) < 0.001)
    }

    test("raizNewton debe manejar correctamente las exponenciaciones") {
        val f = Resta(Expo(x, two), three) // x^2 - 3
        val root = newton.raizNewton(f, x, 1.0, newton.buenaAprox)
        assert(Math.abs(root - Math.sqrt(3)) < 0.001)
    }

    test("raizNewton debe manejar correctamente los logaritmos") {
        val f = Resta(Logaritmo(x), one) // log(x) - 1
        val root = newton.raizNewton(f, x, 2.0, newton.buenaAprox)
        assert(Math.abs(root - Math.E) < 0.001)
    }

    test("buenaAprox debe determinar correctamente las buenas aproximaciones para las expresiones complejas") {
        val f = Resta(Expo(x, two), three) // x^2 - 3
        assert(newton.buenaAprox(f, x, Math.sqrt(3)))
        assert(!newton.buenaAprox(f, x, 2.0))
    }

    test("buenaAprox debe manejar correctamente los números negativos") {
        val f = Resta(Expo(x, two), two) // x^2 - 2
        assert(newton.buenaAprox(f, x, -Math.sqrt(2)))
        assert(!newton.buenaAprox(f, x, -2.0))
    }

    test("buenaAprox debe manejar correctamente las divisiones") {
        val f = Resta(Div(x, two), one) // x/2 - 1
        assert(newton.buenaAprox(f, x, 2.0))
        assert(!newton.buenaAprox(f, x, 1.0))
    }

    test("buenaAprox debe manejar correctamente las exponenciaciones") {
        val f = Resta(Expo(x, two), three) // x^2 - 3
        assert(newton.buenaAprox(f, x, Math.sqrt(3)))
        assert(!newton.buenaAprox(f, x, 2.0))
    }

    test("buenaAprox debe manejar correctamente los logaritmos") {
        val f = Resta(Logaritmo(x), one) // log(x) - 1
        assert(newton.buenaAprox(f, x, Math.E))
        assert(!newton.buenaAprox(f, x, 2.0))
    }
}
