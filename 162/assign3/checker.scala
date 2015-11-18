import scala.io._
import cs162.assign3.syntax._
import Aliases._
import scala.io.Source.fromFile

//——————————————————————————————————————————————————————————————————————————————
// Main entry point

object Checker {
  type TypeEnv = scala.collection.immutable.HashMap[Var, Type]
  object Illtyped extends Exception

  var typeDefs = Set[TypeDef]()

  def main( args:Array[String] ) {
    val filename = args(0)
    val input = fromFile(filename).mkString
    Parsers.program.run(input, filename) match {
      case Left(e) => println(e)
      case Right(program) =>
        val prettied = Pretty.prettySyntax(program)
        typeDefs = program.typedefs

       // try {
          getType( program.e, new TypeEnv())
          println("This program is well-typed:\n")
          println(Pretty.prettySyntax(program))
       // }

        //catch { case Illtyped => println("program is ill-typed") }
    }
  }

  // Gets all the constructors associated with a given type name.
  // For example, consider the following typedefs:
  //
  // type Either = Left num | Right bool
  // type Maybe = Some num | None
  //
  // With respect to the above typedefs, `constructors` will return
  // the following underneath the given arguments:
  //
  // constructors(Label("Either")) = Map(Label("Left") -> NumT, Label("Right") -> BoolT)
  // constructors(Label("Maybe")) = Map(Label("Some") -> NumT, Label("None") -> UnitT)
  // constructors(Label("Fake")) throws Illtyped
  //
  def constructors(name: Label): Map[Label, Type] =
    typeDefs.find(_.name == name).map(_.constructors).getOrElse(throw Illtyped)

  def getType( e:Exp, env:TypeEnv ): Type =
    e match {
      // variables
      case x:Var => env.getOrElse(x,throw Illtyped) // FILL ME IN

      // numeric literals
      case _:Num => NumT // FILL ME IN

      // boolean literals
      case _:Bool => BoolT // FILL ME IN

      // `nil` - the literal for unit
      case _: NilExp => UnitT // FILL ME IN

      // builtin arithmetic operators
      case Plus | Minus | Times | Divide => FunT(Seq(NumT,NumT), NumT) // FILL ME IN

      // builtin relational operators
      case LT | EQ => FunT(Seq(NumT,NumT), BoolT) // FILL ME IN

      // builtin logical operators
      case And | Or => FunT(Seq(BoolT,BoolT), BoolT) // FILL ME IN

      // builtin logical operators
      case Not => FunT(Seq(BoolT), BoolT) // FILL ME IN

      // function creation
      case Fun(params, body) => {
         val t2 = getType(body, params.foldLeft(env)((accum:TypeEnv, t:(Var,Type)) => accum + (t._1 -> t._2)))
         FunT(params.foldLeft(Seq[Type]())((accum:Seq[Type], t:(Var,Type)) => accum:+t._2), t2)
       }

      // function call
      case Call(fun, args) => getType(fun, env) match {
        case FunT(params, body) => {
            val e2ty = args.foldRight(Seq[Type]())((e:Exp, accum:Seq[Type]) => getType(e, env)+:accum)
            if (e2ty == params) body
            else throw Illtyped
        }
        case _ => throw Illtyped
      } 

      // conditionals 
      case If(e1, e2, e3) => getType(e1, env) match {
        case BoolT => {
          val a = getType(e2, env)
          val b = getType(e3, env)
          if(a == b) a 
          else throw Illtyped
        }
        case _ => throw Illtyped
      } 

      // let binding
      case Let(x, e1, e2) => {
        val e1Type = getType(e1,env)
        val e2Type = getType(e2, env + (x -> e1Type))
        e2Type
      } 

      // recursive binding
      case Rec(x, t1, e1, e2) => {
        val e1Type = getType(e1, env + (x -> t1))
        val e2Type = getType(e2, env + (x -> t1))
        if(e1Type == t1) e2Type else throw Illtyped
      }
      

      // record literals
      case Record(fields) => {
        val field = fields.map((t:(Label,Exp)) => (t._1 -> getType(t._2,env)))
        RcdT(field)
      }


      // record access
      case Access(e, field) => getType(e, env) match {
        case RcdT(fields) => {
          fields.getOrElse(field, throw Illtyped)
        }
        case _ => throw Illtyped
      } // FILL ME IN

      // constructor use
      case Construct(name, constructor, e) => {
        val t1 = constructors(name).getOrElse(constructor, throw Illtyped)
        val eType = getType(e, env)
        if(t1 == eType) TypT(name) 
        else throw Illtyped
      } // FILL ME IN

      // pattern matching (case ... of ...)
      case Match(e, cases) => getType(e,env) match {
        case TypT(nombre) => {
          val t0 = cases.map((t:(Label,Var,Exp)) => t._1)
          val t1 = cases.map((t:(Label,Var,Exp)) => getType(t._3, env + (t._2 -> constructors(nombre).getOrElse(t._1, throw Illtyped))))
          if(t0.distinct.sorted != constructors(nombre).map((t:(Label,Type)) => t._1).toSeq.sorted || t0.distinct.length != t0.length ) throw Illtyped
          else if(t1.distinct.length == 1) t1.head
          else throw Illtyped
        }
        case _ => throw Illtyped
      }
    }
}
