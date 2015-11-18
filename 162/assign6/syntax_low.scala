package miniprolog.syntax.lowlevel

object Rhs {
  def rhsvars(rhs: Rhs): Set[Variable] = {
    rhs match {
      case x: Variable => Set(x)
      case _: Num => Set()
      case Structure(_, vars) => vars.toSet
      case Binop(left, _, right) =>
        rhsvars(left) ++ rhsvars(right)
    }
  }
}
import Rhs.rhsvars

sealed trait Rhs
sealed trait Exp extends Rhs
case class Variable(name: String) extends Exp
case class Num(n: Int) extends Rhs

case class Program(clauses: Seq[Clause], query: Query)
case class Query(vars: Seq[Variable], body: Body)
case class Clause(name: String, params: Seq[Variable], localVars: Seq[Variable], body: Body)
case class Structure(name: String, vars: List[Variable]) extends Rhs

sealed trait Body {
  def vars(): Set[Variable]
}
case class And(b1: Body, b2: Body) extends Body {
  def vars() = b1.vars ++ b2.vars
}
case class Or(b1: Body, b2: Body) extends Body {
  def vars() = b1.vars ++ b2.vars
}
case class Unify(var1: Variable, var2: Variable) extends Body {
  def vars() = Set(var1, var2)
}
case class Check(name: String, params: Seq[Variable]) extends Body {
  def vars() = params.toSet
}
case class Compare(var1: Variable, op: ROP, var2: Variable) extends Body {
  def vars() = Set(var1, var2)
}
case class Bind(to: Variable, rhs: Rhs) extends Body {
  def vars() = rhsvars(rhs) + to
}
case class Print(what: Variable) extends Body {
  def vars() = Set(what)
}
case object True extends Body {
  def vars() = Set()
}
case object False extends Body {
  def vars() = Set()
}

case class Binop(left: Exp, op: BOP, right: Exp) extends Exp

class ConvertOp[T](val failMsg: String, val mapping: Map[String, T]) {
  def apply(s: String): T =
    mapping.get(s).getOrElse(sys.error(failMsg + ": " + s))
}

object BOP extends ConvertOp[BOP](
  "Invalid binary operator",
  Map("+" -> Plus, "-" -> Minus, "*" -> Mul, "/" -> Div))

sealed trait BOP
case object Plus extends BOP
case object Minus extends BOP
case object Mul extends BOP
case object Div extends BOP

object ROP extends ConvertOp[ROP](
  "Invalid relational operator",
  Map(">" -> GT, "<" -> LT, "=<" -> LE, ">=" -> GE, 
      "=:=" -> EQ, "=/=" -> NE))

sealed trait ROP
case object LT extends ROP
case object LE extends ROP
case object GT extends ROP
case object GE extends ROP
case object EQ extends ROP
case object NE extends ROP

