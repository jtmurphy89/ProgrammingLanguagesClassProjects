import miniprologfd.solver._
import miniprologfd.interpreter.{ Value, NumValue, SymPlaceholder ⇒ Variable }
import miniprologfd.syntax.lowlevel.{ SymbolicROP ⇒ ROP, SymEQ, SymNE, SymLT,
  SymLE, SymGT, SymGE }

case class TestConstraint(v1: Value, op: ROP, v2: Value) {
  // v1 == num -> v2 != num
  // v1 != num || v2 != num
  assert(!v1.isInstanceOf[NumValue] || !v2.isInstanceOf[NumValue])

  def variables(): Set[Variable] = {
    (v1, v2) match {
      case (x1: Variable, x2: Variable) => Set(x1, x2)
      case (x1: Variable, _) => Set(x1)
      case (_, x1: Variable) => Set(x1)
      case _ => Set()
    }
  }

  def satisfied(mapping: Map[Variable, Int]): Boolean = {
    def eval(v: Value): Option[Int] = {
      v match {
        case NumValue(n) => Some(n)
        case x: Variable if mapping.contains(x) => Some(mapping(x))
        case _ => None
      }
    }

    (for {
      l <- eval(v1)
      r <- eval(v2)
    } yield {
      op match {
        case SymEQ => l == r
        case SymNE => l != r
        case SymLT => l < r
        case SymLE => l <= r
        case SymGT => l > r
        case SymGE => l >= r
      }
    }).getOrElse(false)
  }

  def opString(): String = {
    op match {
      case SymEQ => "="
      case SymNE => "!="
      case SymLT => "<"
      case SymLE => "<="
      case SymGT => ">"
      case SymGE => ">="
    }
  }

  def prettyString(): String = {
    def valToString(v: Value): String = {
      v match {
        case NumValue(n) => n.toString
        case Variable(x) => "x" + (x + 1)
        case _ =>
          throw new Exception("Expected number or variable, saw: " + v)
      }
    }

    valToString(v1) + " " + opString + " " + valToString(v2)
  }
}

case class TestCase(constraints: List[TestConstraint], isSat: Boolean) {
  def variables(): Set[Variable] = {
    constraints.foldLeft(Set[Variable]())((res, cur) =>
      res ++ cur.variables)
  }

  def satisfied(mapping: Map[Variable, Int]): Boolean = {
    constraints.forall(_.satisfied(mapping))
  }

  def prettyString(): String = {
    constraints.map(_.prettyString).mkString(", ")
  }

  def failedString(): String = {
    val satMsg =
      if (isSat) "Should be SAT, was UNSAT" else "Should be UNSAT, was SAT"
    "FAILED: " + prettyString + "\n\t" + satMsg
  }

  def timeoutString(): String = {
    "TIMEOUT: " + prettyString
  }

  def threwExceptionString(): String = {
    "THREW EXCEPTION: " + prettyString
  }
}

sealed trait TestResult
case object Pass extends TestResult
case object Fail extends TestResult
case object ThrewException extends TestResult
case object Timeout extends TestResult

object SolverTests {
  def runTest(test: TestCase): TestResult = {
    import scala.concurrent._
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global

    try {
      if (Await.result(future(runTestWithoutTimeout(test)), 100.millis)) Pass else Fail
    } catch {
      case _: TimeoutException => Timeout
      case _: Throwable => ThrewException
    }
  }

  // true for pass, false for failure
  def runTestWithoutTimeout(test: TestCase): Boolean = {
    import scala.annotation.tailrec

    @tailrec
    def recur(constraints: List[TestConstraint], cs: ConstraintStore): Boolean = {
      constraints match {
        case TestConstraint(v1, op, v2) :: rest =>
          cs.insert(op, v1, v2) match {
            case Some(newCs) => recur(rest, newCs)
            case None => {
              // We hit unsatisfiability.  Make sure we were supposed to
              if (test.isSat) false else true
            }
          }
        case Nil => {
          if (test.isSat) {
            // A satisfying instance exists.  Make sure the instance
            // returned is, in fact, satisfying
            val variables = test.variables.toList
            if (variables.nonEmpty) {
              if (test.satisfied(variables.zip(cs.SAT(variables).map(_.n)).toMap)) {
                true // satisfying solution
              } else {
                false // said SAT, but the solution wasn't satisfying
              }
            } else {
              true // empty set of variables - should only be possible
                   // with an empty set of constraints
            }
          } else {
            false // we were supposed to be unsat, but we said SAT
          }
        }
      }
    }

    recur(test.constraints, ConstraintStore())
  }

  def doTesting(tests: Seq[TestCase]) {
    val (passed, failed, timeout, threwException) = tests.par.foldLeft((List[TestCase](), List[TestCase](), List[TestCase](), List[TestCase]()))(
      { case ((passed, failed, timeout, threwException), test) => {
          runTest(test) match {
            case Pass => (test :: passed, failed, timeout, threwException)
            case Fail => {
              println(test.failedString)
              (passed, test :: failed, timeout, threwException)
            }
            case Timeout => {
              println(test.timeoutString)
              (passed, failed, test :: timeout, threwException)
            }
            case ThrewException => {
              println(test.threwExceptionString)
              (passed, failed, timeout, test :: threwException)
            }
          }
      }
     })

    println("Number passed: " + passed.length)
    println("Number failed: " + failed.length)
    println("Number timed out: " + timeout.length)
    println("Number threw exception: " + threwException.length)
  }

  def main(args: Array[String]) {
    doTesting(Tests.tests)
  }
}

object Tests {
  val x0 = Variable()
  val x1 = Variable()
  val x2 = Variable()
  val x3 = Variable()
  val x4 = Variable()
  val x5 = Variable()
  val x6 = Variable()
  val x7 = Variable()

  val tests =
    Seq(
      TestCase(List(TestConstraint(x1, SymEQ, NumValue(0)),
                    TestConstraint(x2, SymEQ, NumValue(1)),
                    TestConstraint(x1, SymEQ, x2)), false),
      TestCase(List(TestConstraint(NumValue(0), SymLE, x1),
                    TestConstraint(NumValue(0), SymLE, x2),
                    TestConstraint(NumValue(2), SymLE, x3),
                    TestConstraint(NumValue(0), SymLE, x4),
                    TestConstraint(NumValue(0), SymLE, x5),
                    TestConstraint(NumValue(0), SymLE, x6),
                    TestConstraint(NumValue(0), SymLE, x7),
                    TestConstraint(NumValue(4), SymGE, x1),
                    TestConstraint(NumValue(4), SymGE, x2),
                    TestConstraint(NumValue(4), SymGE, x3),
                    TestConstraint(NumValue(4), SymGE, x4),
                    TestConstraint(NumValue(4), SymGE, x5),
                    TestConstraint(NumValue(4), SymGE, x6),
                    TestConstraint(NumValue(4), SymGE, x7),
                    TestConstraint(x3, SymNE, x5),
                    TestConstraint(x6, SymNE, x7),
                    TestConstraint(x4, SymEQ, x5),
                    TestConstraint(x1, SymGT, x6),
                    TestConstraint(x1, SymGT, x7),
                    TestConstraint(x2, SymGT, x1)), true),
      TestCase(List(TestConstraint(NumValue(0), SymLE, x1),
                    TestConstraint(NumValue(0), SymLE, x2),
                    TestConstraint(NumValue(3), SymLE, x3),
                    TestConstraint(NumValue(0), SymLE, x4),
                    TestConstraint(NumValue(9), SymGE, x1),
                    TestConstraint(NumValue(9), SymGE, x2),
                    TestConstraint(NumValue(9), SymGE, x3),
                    TestConstraint(NumValue(9), SymGE, x4),
                    TestConstraint(x1, SymGE, x2),
                    TestConstraint(x2, SymNE, x3),
                    TestConstraint(x3, SymLE, x1),
                    TestConstraint(x1, SymEQ, x4),
                    TestConstraint(x4, SymGE, x2)), true),

      // bounds-checking tests
      TestCase(List(TestConstraint(x0, SymLT, NumValue(0))), false),
      TestCase(List(TestConstraint(NumValue(0), SymGT, x0)), false),
      TestCase(List(TestConstraint(x0, SymEQ, NumValue(-1))), false),
      TestCase(List(TestConstraint(NumValue(-1), SymEQ, x0)), false),
      TestCase(List(TestConstraint(x0, SymEQ, NumValue(501))), false),
      TestCase(List(TestConstraint(NumValue(501), SymEQ, x0)), false),
      TestCase(List(TestConstraint(x0, SymLT, NumValue(501))), true),
      TestCase(List(TestConstraint(x0, SymLE, NumValue(-1))), false),
      TestCase(List(TestConstraint(x0, SymLE, NumValue(501))), true),
      TestCase(List(TestConstraint(x0, SymGT, NumValue(-1))), true),
      TestCase(List(TestConstraint(x0, SymGT, NumValue(500))), false),
      TestCase(List(TestConstraint(x0, SymGE, NumValue(-1))), true),
      TestCase(List(TestConstraint(x0, SymGE, NumValue(501))), false),

      // automatically generated test cases follow
      TestCase(List(TestConstraint(NumValue(229),SymLE,x1), TestConstraint(x2,SymNE,x1)),true),
      TestCase(List(TestConstraint(x1,SymEQ,x1), TestConstraint(NumValue(222),SymLE,x1)),true),
      TestCase(List(TestConstraint(x1,SymEQ,NumValue(169)), TestConstraint(x2,SymNE,x1)),true),
      TestCase(List(TestConstraint(x1,SymLT,x2), TestConstraint(NumValue(416),SymLT,x3)),true),
      TestCase(List(TestConstraint(NumValue(229),SymGT,x1), TestConstraint(x1,SymLE,NumValue(388))),true),
      TestCase(List(TestConstraint(x1,SymLE,x2), TestConstraint(x1,SymLT,NumValue(173))),true),
      TestCase(List(TestConstraint(NumValue(229),SymGE,x1), TestConstraint(NumValue(267),SymGE,x2)),true),
      TestCase(List(TestConstraint(x1,SymGE,x2), TestConstraint(x2,SymLT,NumValue(456))),true),
      TestCase(List(TestConstraint(x1,SymNE,NumValue(353)), TestConstraint(x2,SymNE,x3)),true),
      TestCase(List(TestConstraint(x1,SymLE,x2), TestConstraint(NumValue(341),SymGT,x3)),true),
      TestCase(List(TestConstraint(NumValue(229),SymEQ,x1), TestConstraint(x1,SymGE,x2)),true),
      TestCase(List(TestConstraint(x1,SymLE,x1), TestConstraint(x1,SymLE,x2)),true),
      TestCase(List(TestConstraint(x1,SymLT,NumValue(150)), TestConstraint(x2,SymEQ,x3)),true),
      TestCase(List(TestConstraint(x1,SymNE,x2), TestConstraint(x2,SymLE,NumValue(332))),true),
      TestCase(List(TestConstraint(x1,SymGT,NumValue(436)), TestConstraint(x2,SymLT,NumValue(168))),true),
      TestCase(List(TestConstraint(NumValue(229),SymGE,x1), TestConstraint(x1,SymLE,NumValue(232))),true),
      TestCase(List(TestConstraint(x1,SymLE,x2), TestConstraint(x2,SymGE,NumValue(396))),true),
      TestCase(List(TestConstraint(NumValue(229),SymEQ,x1), TestConstraint(NumValue(473),SymGE,x2)),true),
      TestCase(List(TestConstraint(x1,SymLE,x2), TestConstraint(x2,SymLT,NumValue(236))),true),
      TestCase(List(TestConstraint(x1,SymNE,x2), TestConstraint(x3,SymGE,x2)),true),
      TestCase(List(TestConstraint(x1,SymEQ,NumValue(169)), TestConstraint(x1,SymEQ,x1)),true),
      TestCase(List(TestConstraint(x1,SymEQ,x1), TestConstraint(x2,SymLE,x1)),true),
      TestCase(List(TestConstraint(x1,SymNE,x2), TestConstraint(NumValue(265),SymEQ,x3)),true),
      TestCase(List(TestConstraint(x1,SymEQ,NumValue(169)), TestConstraint(NumValue(404),SymNE,x2)),true),
      TestCase(List(TestConstraint(x1,SymLE,NumValue(23)), TestConstraint(NumValue(96),SymGT,x1)),true),
      TestCase(List(TestConstraint(x1,SymGE,x2), TestConstraint(x3,SymLE,x2)),true),
      TestCase(List(TestConstraint(x1,SymEQ,x2), TestConstraint(x1,SymEQ,x2)),true),
      TestCase(List(TestConstraint(NumValue(229),SymGE,x1), TestConstraint(x1,SymGE,x1)),true),
      TestCase(List(TestConstraint(x1,SymGE,x1)),true),
      TestCase(List(TestConstraint(x1,SymEQ,x2)),true),
      TestCase(List(TestConstraint(x1,SymGT,NumValue(190))),true),
      TestCase(List(TestConstraint(x1,SymEQ,NumValue(73))),true),
      TestCase(List(TestConstraint(x1,SymLT,x2)),true),
      TestCase(List(TestConstraint(x1,SymNE,NumValue(176))),true),
      TestCase(List(TestConstraint(x1,SymEQ,x1)),true),
      TestCase(List(TestConstraint(NumValue(373),SymEQ,x1)),true),
      TestCase(List(TestConstraint(NumValue(373),SymNE,x1)),true),
      TestCase(List(TestConstraint(x1,SymGE,x2)),true),
      TestCase(List(TestConstraint(NumValue(373),SymGT,x1)),true),
      TestCase(List(TestConstraint(x1,SymLE,x2)),true),
      TestCase(List(TestConstraint(x1,SymLT,NumValue(98))),true),
      TestCase(List(TestConstraint(x1,SymLE,x1)),true),
      TestCase(List(TestConstraint(NumValue(373),SymLT,x1)),true),
      TestCase(List(TestConstraint(NumValue(373),SymGE,x1)),true),
      TestCase(List(TestConstraint(NumValue(373),SymLE,x1)),true),
      TestCase(List(TestConstraint(x1,SymLE,NumValue(445))),true),
      TestCase(List(TestConstraint(x1,SymNE,x2)),true),
      TestCase(List(TestConstraint(x1,SymGE,NumValue(59))),true))
}
