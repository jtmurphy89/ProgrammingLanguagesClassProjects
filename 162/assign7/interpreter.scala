package miniprologfd.interpreter

import miniprologfd.syntax.lowlevel._
import miniprologfd.solver._

// There are four kinds of values in this language:
// 1.) Integers, represented by NumValue
// 2.) Structures which contain values, represented by Constructor
// 3.) A special placeholder for some other (possibly placeholder)
//     value. This is represented by Placeholder.
// 4.) Symbolic placeholders for symbolic constraints.
//
// Note that in the code comments, 'placeholder' refers to the third
// type of value (a non-symbolic placeholder) unless explicitly
// referred to as a 'symbolic placeholder'.
sealed trait Value
case class NumValue(n: Int) extends Value
case class Constructor(name: String, values: List[Value]) extends Value
case class Placeholder(id: Int) extends Value
case class SymPlaceholder(id: Int) extends Value

trait WithCounter[T] {
  private var counter = 0
  def apply(): T = {
    val retval = makeFresh(counter)
    counter += 1
    retval
  }

  protected def makeFresh(x: Int): T
}
    
// Used to create a new Placeholder. This should be the only place
// used to create new placeholders, which should all have unique ids.
object Placeholder extends WithCounter[Placeholder] {
  protected def makeFresh(x: Int) = Placeholder(x)
}

// Used to create a new SymPlaceholder. This should be the only place
// used to create new symbolic placeholders.
object SymPlaceholder extends WithCounter[SymPlaceholder] {
  protected def makeFresh(x: Int) = SymPlaceholder(x)
}

// Representation of equivalence classes.  This maps
// placeholders to other, possibly placeholder, values.
// This allows for variable aliasing to occur.  For example, where
// P(1) means a placeholder with ID 1, with the following mapping:
// [P(1) -> P(2), P(2) -> P(3), P(4) -> NumValue(7)]
// ...P(1), P(2), and P(3) are all in the same equivalence class.
//
// Equivalence classes contain at most one non-placeholder value.
// It is guaranteed that a lookup on an equivalence class which
// contains a non-placeholder value will return the placeholder
// value.
//
// It is also assumed that whenever a variable lookup occurs, the
// equivalence relation will be consulted before the variable's value
// is considered.  For example, the environment may be like so:
// [Variable("X") -> P(0)]
// ...but the equivalence relation may be like so:
// [P(0) -> P(1), P(1) -> NumValue(42)]
// ...in this example, the value of the variable "X" should be
// the number 42, even though the environment doesn't map directly
// to 42 (it maps to 42 indirectly through the equivalence relation).
class EquivalenceRelation(val mapping: Map[Placeholder, Value] = Map()) {
  // Gets the set representative for the value `v`.
  // if `v` is a placeholder, then it should lookup whatever `v` maps
  // to recursively.  If `v` is a non-placeholder, then it should
  // simply return `v` as-is.
  def lookup(v: Value): Value = ??? // FILL ME IN

  // Adds a relation between `p` and `v`.  It should be the case that both
  // are set representatives.
  def addRelation(p: Placeholder, v: Value): EquivalenceRelation = ??? // FILL ME IN
}

object Interpreter {
  import miniprologfd.parser._
  import miniprologfd.translator._

  // Takes a message to abort with.  The message is for your own debugging
  // purposes; we will not be using the message during grading
  def abortInterpreter(message: String): Nothing = {
    throw new Exception(message)
  }

  // Given a list of clauses, it returns a mapping of clause name, arity
  // pairs to clauses with that name and arity.  The clauses should be
  // in the same order as in the file.  It should be guaranteed that
  // there are no empty lists of clauses.
  def toClauseDB(clauses: Seq[Clause]): Map[(String, Int), List[Clause]] = ??? // FILL ME IN

  def makeQuery(rawQuery: String): Query =
    Translator.translateQuery(Parser.parseQuery(rawQuery))

  def makeInterpreter(clausesFile: String, query: String): Interpreter[NormalPrinter.type] = {
    import scala.io._
    val highlevel = Parser.parseClauses(Source.fromFile(clausesFile).mkString)
    val lowlevel = highlevel.map(Translator.translateClause)
    new Interpreter(Program(lowlevel, makeQuery(query)), NormalPrinter)
  }

  def main(args: Array[String]) {
    if (args.length != 2) {
      println(
        "Needs a file holding clauses and a query on the command line")
    } else {
      makeInterpreter(args(0), args(1)).run()
    }
  }
}

sealed trait Printer {
  def doPrint(what: String): Unit
}
object NormalPrinter extends Printer {
  def doPrint(what: String) {
    println(what)
  }
}
class TestingPrinter extends Printer {
  val outputs = scala.collection.mutable.ArrayBuffer.empty[String] // for tests generation

  def doPrint(what: String) {
    outputs += what
  }
}

// An interpreter is passed a program, and subsequently sets up
// an internal clause database.  The query held within the program
// is run by calling the run() method of the interpreter.
class Interpreter[P <: Printer](val program: Program, val printer: P) {
  import scala.collection.immutable.Stack
  import Interpreter.abortInterpreter

  // Type alias; with this declaration "Env" is synonymous with
  // the bulkier (and less informative) type Map[Variable, Value]
  type Env = Map[Variable, Value]

  // Representation for goals.  A goal is either a body (BodyGoal)
  // or the special restore goal (RestoreGoal) for restoring the
  // caller's environment after a clause call.
  sealed trait Goal
  case class BodyGoal(body: Body) extends Goal
  case class RestoreGoal(env: Env) extends Goal

  type GoalStack = Stack[Goal]
  type Choice = (Body, Env, EquivalenceRelation, GoalStack, ConstraintStore)
  type ChoiceStack = Stack[Choice]

  // the clause database
  val db: Map[(String, Int), List[Clause]] =
    Interpreter.toClauseDB(program.clauses)

  // Creates a new environment, given:
  // 1.) The local variables for the given environment
  // 2.) Variable/value pairs corresponding to values passed in.
  //     For example, for a clause foo(X, Y), if we call
  //     foo(3, 4), then we'd pass Seq(X -> 3, Y -> 4).
  //
  // It is assumed that there are no variables in common between the
  // local variables and the passed variables.  The translation guards
  // against this; for example, with the following:
  // foo(X) :- ... .
  // ?- X = 1, foo(X).
  // ...the translator performs certain renamings that prevent any
  // conflicts here.  It is, however, possible to pass incorrect
  // information to `newEnv`.
  def newEnv(local: Seq[Variable], passed: Seq[(Variable, Value)]): Env = ??? // FILL ME IN

  // Attempts to unify v1 and v2, which are assumed to be set representatives.
  // Returns Some(equiv) if they could be unified; in this case, equiv is
  // the resulting possibly new equivalence relation.  Otherwise unify
  // returns None, indicating that v1 and v2 could not be unified.
  //
  // This version is extended from the mini-prolog version to also
  // take and return a constraint store, so that it can appropriately
  // handle unification of symbolic placeholders.  Be sure to handle
  // these additional cases, as per the specification:
  // 1.) Symbolic placeholders with numbers
  // 2.) Numbers with symbolic placeholders
  // 3.) Symbolic placeholders with other symbolic placeholders
  def unify(v1: Value, v2: Value, equiv: EquivalenceRelation, cs: ConstraintStore): Option[(EquivalenceRelation, ConstraintStore)] = ??? // FILL ME IN

  // Unifies a sequence of possibly non-set representative values with
  // respect to the provided equivalence relation.  Behaves largely as
  // a helper function to unify.
  def unifyTerms(pairs: Seq[(Value, Value)], equiv: EquivalenceRelation): Option[EquivalenceRelation] = ??? // FILL ME IN

  // Given some value which is assumed to be a set representative, it
  // will build a string representing what should be printed for the
  // value.  For a variable with ID 5, it will simply print _5.
  // For integers, it will print the integer.  For constructors, it
  // will print like so: constructorName(...), where ... represents
  // a recursive buildPrint call for each value.  Has special handling
  // for lists, to make the output easier to read (and for greater
  // consistency with Prolog).
  def buildPrint(v: Value, env: Env, equiv: EquivalenceRelation, inList: Boolean = false): String = {
    import miniprologfd.translator.Translator.{CONS_NAME, NIL_NAME}

    v match {
      case NumValue(n) => n.toString
      case Placeholder(n) => "_" + n.toString
      case Constructor(name, Nil) => {
        if (name == NIL_NAME) {
          if (inList) "]" else "[]"
        } else {
          name
        }
      }
      case Constructor(CONS_NAME, head :: tail :: Nil) => {
        val start = if (inList) ", " else "["
        (start + buildPrint(equiv.lookup(head), env, equiv) + 
         buildPrint(equiv.lookup(tail), env, equiv, true))
      }
      case Constructor(name, vars) => {
        (name + "(" + vars.map(v => buildPrint(equiv.lookup(v), env, equiv)).mkString(", ") + ")")
      }
      case SymPlaceholder(n) => "#" + n.toString
    }
  }

  // Evaluates the given expression down to an integer.
  // If the expression involves a non-integer or an attempt
  // to divide by zero is made, then this will call
  // `abortInterpreter` with an appropriate error message.
  // This should largely be the same as the previous assignment,
  // since this only deals with concrete integers (not symbolic
  // ones)
  def eval(exp: Exp, env: Env, equiv: EquivalenceRelation): Int = ??? // FILL ME IN

  // Compares two integers with respect to the given operator.
  // This only deals with concrete relational operations, as with
  // the previous assignment
  def compare(n1: Int, op: ConcreteROP, n2: Int): Boolean = ??? // FILL ME IN

  // Actually runs the interpreter's query.
  def run() {
    // setting up of local variables
    var env: Env = newEnv(program.query.vars, Seq())
    var equiv: EquivalenceRelation = new EquivalenceRelation()
    var goalStack: GoalStack = Stack(BodyGoal(program.query.body))
    var choiceStack: ChoiceStack = Stack()
    var constraintStore: ConstraintStore = ConstraintStore() // A3

    // turn a symbolic relational operator into a concrete one
    def symToConcrete( op:SymbolicROP ): ConcreteROP =
      op match {
        case SymLT => LT
        case SymLE => LE
        case SymGT => GT
        case SymGE => GE
        case SymEQ => EQ
        case SymNE => NE
      }

    // extract a list of symbolic placeholders contained in a Value
    def getSymPs( v:Value ): List[SymPlaceholder] =
      v match {
        case sp:SymPlaceholder => List(sp)
        case Constructor(_, vs) => vs flatMap (getSymPs(_))
        case _ => List()
      }

    while (goalStack.nonEmpty) {
      val (goal, newGoalStack) = goalStack.pop2
      goalStack = newGoalStack
      goal match {
        case BodyGoal(And(body1, body2)) => ??? // FILL ME IN

        case BodyGoal(Or(body1, body2)) => ??? // FILL ME IN

        case BodyGoal(Unify(x1, x2)) => ??? // FILL ME IN

        case BodyGoal(Check(name, vars)) => ??? // FILL ME IN

        case RestoreGoal(envR) => ??? // FILL ME IN

        case BodyGoal(ConcreteCompare(x1, op, x2)) => ??? // FILL ME IN

        case BodyGoal(Bind(x, rhs)) => ??? // FILL ME IN

        case BodyGoal(True) => ()

        case BodyGoal(False) => ??? // FILL ME IN

        case BodyGoal(SymbolicCompare(x1, op, x2)) => ??? // FILL ME IN

        case BodyGoal(FDLabeling(x)) => ??? // FILL ME IN

        case BodyGoal(Print(x)) => {
          printer.doPrint(buildPrint(equiv.lookup(env(x)), env, equiv, false))
        }
      } // goalStack.pop2
    } // while
  } // run
} // Interpreter

