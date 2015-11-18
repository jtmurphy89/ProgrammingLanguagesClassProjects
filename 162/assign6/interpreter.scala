package miniprolog.interpreter

import miniprolog.syntax.lowlevel._

// There are three kinds of values in this language:
// 1.) Integers, represented by NumValue
// 2.) Structures which contain values, represented by Constructor
// 3.) A special placeholder for some other (possibly placeholder)
//     value.  This is represented by Placeholder.
sealed trait Value
case class NumValue(n: Int) extends Value
case class Constructor(name: String, values: List[Value]) extends Value
case class Placeholder(id: Int) extends Value

// Used to create a new Placeholder.  This should be the only
// place used to create new placeholders, which should all
// have unique ids.
object Placeholder {
  private var counter = 0
  def apply(): Placeholder = {
    val retval = Placeholder(counter)
    counter += 1
    retval
  }
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
// contains a non-placeholder value will return the
// non-placeholder value.
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
class EquivalenceRelation(val mapping: Map[Placeholder, Value]) {
  // Gets the set representative for the value `v`.
  // if `v` is a placeholder, then it should lookup whatever `v` maps
  // to recursively.  If `v` is a non-placeholder, then it should
  // simply return `v` as-is.
  def lookup(v: Value): Value = {
    v match {
      case x:Placeholder => if(mapping.get(x) == None) x else lookup(mapping(x))
      case _ => v
    }
  }

  // Adds a relation between `p` and `v`.  It should be the case that both
  // are set representatives.
  def addRelation(p: Placeholder, v: Value): EquivalenceRelation = new EquivalenceRelation(mapping + ((p,v)))
}

object Interpreter {
  import miniprolog.parser._
  import miniprolog.translator._

  // Takes a message to abort with.  The message is for your own debugging
  // purposes; we will not be using the message during grading
  def abortInterpreter(message: String): Nothing = {
    throw new Exception(message)
  }

  // Given a list of clauses, it returns a mapping of clause name, arity
  // pairs to clauses with that name and arity.  The clauses should be
  // in the same order as in the file.  It should be guaranteed that
  // there are no empty lists of clauses.
  def toClauseDB(clauses: Seq[Clause]): Map[(String, Int), List[Clause]] = clauses.foldRight(Map[(String, Int), List[Clause]]())(
    (c:Clause, accum:Map[(String, Int), List[Clause]]) => 
    accum + (((c.name,c.params.length), c::accum.getOrElse((c.name,c.params.length), List[Clause]()))))
    


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
  type Choice = (Body, Env, EquivalenceRelation, GoalStack)
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
  def newEnv(local: Seq[Variable], passed: Seq[(Variable, Value)]): Env = {
    local.foldLeft(Map[Variable, Value]())((accum:Map[Variable, Value], c:Variable) => accum + ((c, Placeholder.apply()))) ++ passed.toMap 
  }



  // Attempts to unify v1 and v2, which are assumed to be set representatives.
  // Returns Some(equiv) if they could be unified; in this case, equiv is
  // the resulting possibly new equivalence relation.  Otherwise unify
  // returns None, indicating that v1 and v2 could not be unified.
  def unify(v1: Value, v2: Value, equiv: EquivalenceRelation): Option[EquivalenceRelation] = (equiv.lookup(v1),equiv.lookup(v2)) match {
    case (x,y) if x == y => Some(equiv)
    case (x:Placeholder, y:Value) => Some(equiv.addRelation(x,y))
    case (x:Value, y:Placeholder) => Some(equiv.addRelation(y,x))
    case (Constructor(nombre1, valor1), Constructor(nombre2, valor2)) => if((nombre1 == nombre2) && (valor1.size == valor2.size)) {unifyTerms(valor1.zip(valor2), equiv)} else None
    case _ => None
  }

  // Unifies a sequence of possibly non-set representative values with
  // respect to the provided equivalence relation.  Behaves largely as
  // a helper function to unify.

  // maybe consider passing an option equivalence relation?
  def unifyTerms(pairs: Seq[(Value, Value)], equiv: EquivalenceRelation): Option[EquivalenceRelation] = pairs.foldLeft(Some(equiv):Option[EquivalenceRelation])( 
    (acc:Option[EquivalenceRelation], x:(Value,Value)) => acc match {
      case None => None
      case Some(newEquiv) => unify(equiv.lookup(x._1), equiv.lookup(x._2), newEquiv)
      }
      )

  // Given some value which is assumed to be a set representative, it
  // will build a string representing what should be printed for the
  // value.  For a variable with ID 5, it will simply print _5.
  // For integers, it will print the integer.  For constructors, it
  // will print like so: constructorName(...), where ... represents
  // a recursive buildPrint call for each value.  Has special handling
  // for lists, to make the output easier to read (and for greater
  // consistency with Prolog).
    def buildPrint(v: Value, env: Env, equiv: EquivalenceRelation, inList: Boolean): String = {
    v match {
      case NumValue(n) => n.toString
      case Placeholder(n) => "_" + n.toString
      case Constructor(name, Nil) => {
        if (name == "[]") {
          if (inList) "]" else "[]"
        } else {
          name
        }
      }
      case Constructor(".", head :: tail :: Nil) => {
        val start = if (inList) ", " else "["
        (start + buildPrint(equiv.lookup(head), env, equiv, false) + 
         buildPrint(equiv.lookup(tail), env, equiv, true))
      }
      case Constructor(name, vars) => {
        (name + "(" + vars.map(v => buildPrint(equiv.lookup(v), env, equiv, false)).mkString(", ") + ")")
      }
    }
  }

  // Evaluates the given expression down to an integer.
  // If the expression involves a non-integer or an attempt
  // to divide by zero is made, then this calls `abortInterpreter`
  // with an appropriate message.
  def eval(exp: Exp, env: Env, equiv: EquivalenceRelation): Int = {
    exp match {
      case x:Variable => {
        equiv.lookup(env(x)) match {
          case NumValue(n) => n
          case _ => abortInterpreter("Variable NAN")
        }
      }
      case Binop(left, op, right) => {
        val l = eval(left, env, equiv)
        val r = eval(right,env,equiv)
        op match {
            case Plus => l + r
            case Minus => l - r
            case Mul => l*r
            case Div => if(r == 0) abortInterpreter("Divide by zero, shit will break...") else l/r
          }
        }
       case _ => abortInterpreter("You didn't enter an Exp")
     }
    }

  // Compares two integers with respect to the given operator.
  def compare(n1: Int, op: ROP, n2: Int): Boolean = {
    op match {
      case LT => (n1 < n2)
      case LE => (n1 <= n2)
      case GT => (n1 > n2)
      case GE => (n1 >= n2)
      case EQ => (n1 == n2)
      case NE => (n1 != n2)
    }
  }

  // Actually runs the interpreter's query.
  def run() {
    // setting up of local variables
    var env: Env = newEnv(program.query.vars, Seq())
    var equiv: EquivalenceRelation = new EquivalenceRelation(Map())
    var goalStack: GoalStack = Stack(BodyGoal(program.query.body))
    var choiceStack: ChoiceStack = Stack()

    while (goalStack.nonEmpty) {
      val (goal, newGoalStack) = goalStack.pop2
      goalStack = newGoalStack
      goal match {
        case BodyGoal(And(body1, body2)) => goalStack = goalStack.push(BodyGoal(body2), BodyGoal(body1))

        case BodyGoal(Or(body1, body2)) => {
          choiceStack = choiceStack.push((body2, env, equiv, goalStack)) 
          goalStack = goalStack.push(BodyGoal(body1))
        }

        case BodyGoal(Unify(x1, x2)) => {
          unify(env(x1),env(x2),equiv) match {
            case None => goalStack = goalStack.push(BodyGoal(False))
            case Some(newEquiv) => equiv = newEquiv
          }
        }

        case BodyGoal(Check(name, vars)) => {
          val c = db(name, vars.length) 
          if(c.isEmpty || c == None) abortInterpreter("Check") 
          else {
            goalStack = goalStack.push(RestoreGoal(env))
            val varList = vars.map((v:Variable) => equiv.lookup(env(v)))
            choiceStack = c.tail.foldRight(choiceStack)((c:Clause, cStack:Stack[Choice]) => { 
              val choice = new Choice(c.body, newEnv(c.localVars, c.params.zip(varList)),equiv,goalStack) 
              cStack.push(choice)
              })
            env = newEnv(c.head.localVars, c.head.params.zip(varList))
            goalStack = goalStack.push(BodyGoal(c.head.body))
          }
        }

        case RestoreGoal(envR) => {env = envR}

        case BodyGoal(Compare(x1, op, x2)) => { (equiv.lookup(env(x1)),equiv.lookup(env(x2))) match {
          case (NumValue(n1), NumValue(n2)) => if(compare(n1,op,n2) == false) goalStack = goalStack.push(BodyGoal(False))
          case _ => abortInterpreter("Compare") 
          }
        }

        case BodyGoal(Bind(x, rhs)) => {rhs match{
          case Num(n) => equiv = equiv.addRelation(equiv.lookup(env(x)).asInstanceOf[Placeholder], NumValue(n))
          case y:Variable => equiv = equiv.addRelation(equiv.lookup(env(x)).asInstanceOf[Placeholder],equiv.lookup(env(y)))
          case e:Exp => equiv = equiv.addRelation(equiv.lookup(env(x)).asInstanceOf[Placeholder], NumValue(eval(e,env,equiv)))
          case Structure(nombre, variables) => equiv = equiv.addRelation(equiv.lookup(env(x)).asInstanceOf[Placeholder], Constructor(nombre, variables.map((v:Variable) => equiv.lookup(env(v)))))
          case _ => abortInterpreter("Bind")
        }
      }

        case BodyGoal(True) => ()

        case BodyGoal(False) => {
          if(choiceStack.isEmpty) { 
            env = Map[Variable,Value]()
            goalStack = Stack()
        }
        else {
          val (choice, choiceStack2) = choiceStack.pop2
          choiceStack = choiceStack2
          env = choice._2
          equiv = choice._3
          goalStack = choice._4
          goalStack = goalStack.push(BodyGoal(choice._1))
        }
      }

        case BodyGoal(Print(x)) => {
          printer.doPrint(buildPrint(equiv.lookup(env(x)), env, equiv, false))
        }
      } // goalStack.pop2
    } // while
  } // run
} // Interpreter

