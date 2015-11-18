package miniprologfd.syntax.lowlevel

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
case class ConcreteCompare(var1: Variable, op: ConcreteROP, var2: Variable) extends Body {
  def vars() = Set(var1, var2)
}
case class SymbolicCompare(var1: Variable, op: SymbolicROP, var2: Variable) extends Body {
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
case class FDLabeling(x: Variable) extends Body {
  def vars() = Set(x)
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
      "=:=" -> EQ, "=/=" -> NE, "#<" -> SymLT,
      "#=<" -> SymLE, "#>" -> SymGT, "#>=" -> SymGE,
      "#=" -> SymEQ, "#\\=" -> SymNE))

sealed trait ROP
sealed trait ConcreteROP extends ROP
case object LT extends ConcreteROP
case object LE extends ConcreteROP
case object GT extends ConcreteROP
case object GE extends ConcreteROP
case object EQ extends ConcreteROP
case object NE extends ConcreteROP

sealed trait SymbolicROP extends ROP
case object SymLT extends SymbolicROP
case object SymLE extends SymbolicROP
case object SymGT extends SymbolicROP
case object SymGE extends SymbolicROP
case object SymEQ extends SymbolicROP
case object SymNE extends SymbolicROP
