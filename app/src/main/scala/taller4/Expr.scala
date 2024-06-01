package taller4

trait Expr
  case class Numero(d: Double) extends Expr
  case class Atomo(x: Char) extends Expr
  case class Suma(e1: Expr,
                  e2: Expr) extends Expr
  case class Prod(e1: Expr,
                  e2: Expr) extends Expr
  case class Resta(e1: Expr,
                   e2: Expr) extends Expr
  case class Div(e1: Expr,
                 e2: Expr) extends Expr
  case class Expo(e1: Expr,
                  e2: Expr) extends Expr
  case class Logaritmo(e1: Expr) extends Expr

object Numero {
  def apply(d: Double) = new Numero(d)
}

object Atomo {
  def apply(a: Char) = new Atomo(a)
}

object Suma {
  def apply(e1: Expr, e2: Expr) = new Suma(e1, e2)
}

object Prod {
  def apply(e1: Expr, e2: Expr) = new Prod(e1, e2)
}

object Resta {
  def apply(e1: Expr, e2: Expr) = new Resta(e1, e2)
}

object Div {
  def apply(e1:Expr,e2:Expr) = new Div(e1,e2)
}

object Expo {
  def apply(e1:Expr,e2:Expr) = new Expo(e1,e2)
}

object Logaritmo {
  def apply(e1:Expr) = new Logaritmo(e1)
}

