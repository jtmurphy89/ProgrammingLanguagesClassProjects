package miniprolog.translator

import miniprolog.syntax.lowlevel._
import miniprolog.syntax.highlevel._

// Translations performed:
// 1.) The parameters to clauses are normalized.  For example:
//
//  foo(X, X, Y) :- rest.
//
//  ...becomes...
//
//  foo(T1, T2, T3) :-
//    T1 = X,
//    T2 = X,
//    T3 = Y,
//    rest.
//
// 2.) Variables are introduced so that unifications are only on variables.
//     For example:
//
//  foo(bar(1,2)) = foo(bar(X, Y))
//
//  ...becomes...
//
//  T1 <- 1,
//  T2 <- 2,
//  T3 <- bar(T1, T2),
//  T4 <- bar(X, Y),
//  T5 <- foo(T3),
//  T6 <- foo(T4),
//  unify T5, T6
// 
// Basic idea: when we translate X, we end up with X' along with a series
// of clauses which must hold for X' to be correct
object Translator {
  // these names below are consistent with actual Prolog
  val CONS_NAME = Symbol(".")
  val NIL_NAME = Symbol("[]")

  private var counter = 0

  def fresh(): String = {
    val retval = counter.toString
    counter += 1
    retval
  }

  def freshVar(): Variable =
    Variable(fresh())

  def freshVariable(): FullVar =
    FullVar(fresh())

  def assignTranslate(what: Rhs, bodies: List[Body]): (Variable, List[Body]) = {
    val x = freshVar()
    (x, Bind(x, what) :: bodies)
  }

  def translateFunctor(f: FullFunctor): (Structure, List[Body]) = {
    val (vars, bodies) = f.params.map(translateTermExp).unzip
    (Structure(f.name.symbol.name, vars), bodies.flatten)
  }

  def translateTermExp(te: FullTermExp): (Variable, List[Body]) = {
    te match {
      case x: FullVar => translateVar(x)
      case a: FullAtom => 
        assignTranslate(Structure(a.symbol.name, List()), List())
      case FullTermNum(n) => assignTranslate(Num(n), List())
      case FullTermFunctor(f) => {
        val (structure, bodies) = translateFunctor(f)
        assignTranslate(structure, bodies)
      }
      case TermList(items) => {
        // translate [1,2,3] => cons(1, cons(2, cons(3, nil))), then translate that
        translateTermExp(
          items.foldRight(FullAtom(NIL_NAME): FullTermExp)((cur, res) =>
            FullTermFunctor(FullFunctor(FullAtom(CONS_NAME), List(cur, res)))))
      }
      case TermListDestruct(head, tail) => {
        // translate [Head|Tail] => cons(Head, Tail), then translate that
        translateTermExp(
          FullTermFunctor(FullFunctor(FullAtom(CONS_NAME), List(head, tail))))
      }
    }
  }

  def directTranslateVar(x: FullVar, underscoreSpecial: Boolean = true): Variable =
    if (underscoreSpecial && x.name == "_") freshVar() else Variable(x.name)

  def translateVar(x: FullVar, underscoreSpecial: Boolean = true): (Variable, List[Body]) =
    (directTranslateVar(x, underscoreSpecial), List())

  def translateRelationalTerm(rt: FullRelationalTerm): (Variable, List[Body]) = {
    rt match {
      case FullExpressionVar(x) => translateTermExp(x)
      case FullExpressionNum(n) => translateTermExp(n)
    }
  }

  def translateExpressionTerm(et: FullExpressionTerm): (Exp, List[Body]) = {
    et match {
      case rt: FullRelationalTerm => translateRelationalTerm(rt)
      case FullExpressionBinop(left, op, right) => {
        val (leftVar, leftBodies) = translateExpressionTerm(left)
        val (rightVar, rightBodies) = translateExpressionTerm(right)
        (Binop(leftVar, op, rightVar), leftBodies ++ rightBodies)
      }
    }
  }

  def toBody(bodies: List[Body]): Body =
    bodies.reduceRight((cur, res) => And(cur, res))

  def translateBody(b: FullBody): List[Body] = {
    b match {
      case FullAnd(b1, b2) =>
        translateBody(b1) ++ translateBody(b2)
      case FullOr(b1, b2) => {
        List(
          Or(toBody(translateBody(b1)),
             toBody(translateBody(b2))))
      }
      case FullUnify(te1, te2) => {
        val (x1, b1) = translateTermExp(te1)
        val (x2, b2) = translateTermExp(te2)
        b1 ++ b2 ++ List(Unify(x1, x2))
      }
      case FullCall(f) => {
        val (Structure(name, vars), bodies) = translateFunctor(f)
        bodies ++ List(Check(name, vars))
      }
      case FullWrite(x) => List(Print(directTranslateVar(x)))
      case FullTrue => List(True)
      case FullFail => List(False)
      case FullCompare(rt1, op, rt2) => {
        val (leftVar, leftBodies) = translateRelationalTerm(rt1)
        val (rightVar, rightBodies) = translateRelationalTerm(rt2)
        leftBodies ++ rightBodies ++ List(Compare(leftVar, op, rightVar))
      }
      case FullArithmetic(lhs, exp) => {
        val (leftVar, leftBodies) = translateRelationalTerm(lhs)
        val (expTrans, rightBodies) = translateExpressionTerm(exp)
        val x = freshVar()
        leftBodies ++ rightBodies ++ List(Bind(x, expTrans), Unify(x, leftVar))
      }
    }
  }

  def translateClause(c: FullClause): Clause = {
    // introduce a new variable for each parameter, and unify with that
    val (newFormal, normalUnifications) =
      c.head.params.map(param => {
        val x = freshVariable()
        (x, FullUnify(x, param))
      }).unzip
    
    val newBody =
      toBody(
        normalUnifications.map(translateBody).flatten ++ translateBody(c.body))
    val lowerFormal = newFormal.map(x => directTranslateVar(x, false))
    Clause(c.head.name.symbol.name, lowerFormal,
           (newBody.vars -- lowerFormal).toSeq, newBody)
  }

  def translateQuery(q: FullQuery): Query = {
    val newBody = toBody(translateBody(q.body))
    Query(newBody.vars.toSeq, newBody)
  }
} // Translator
    
    
